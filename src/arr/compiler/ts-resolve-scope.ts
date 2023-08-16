import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as AU from './ts-ast-util';
import type * as TJ from './ts-codegen-helpers';
import type { List, Option, MutableStringDict, PFunction, StringDict, PMethod, Runtime } from './ts-impl-types';

export type Exports = {
  dict: {
    values: {
      dict: {
        'desugar-scope': PFunction<(program: A.Program, env: CS.CompileEnvironment, options : any) => CS.ScopeResolution>
        'resolve-names': PFunction<(program: A.Program, thismodulesUri: string, initialEnv: CS.CompileEnvironment) => CS.NameResolution>
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-ast-util']},
 ],
  nativeRequires: [],
  provides: {
    values: {
      "desugar-scope": "tany",
      "resolve-names": "tany"
    }
  },
  theModule: function(runtime: Runtime, _, __, tj : TJ.Exports, TS : (TS.Exports), Ain : (A.Exports), CSin : (CS.Exports), AUin : (AU.Exports)) {
    const A = Ain.dict.values.dict;
    const AU = AUin.dict.values.dict;
    const CS = CSin.dict.values.dict;
    const {
      listToArray,
      InternalCompilerError,
      MakeName
    } = tj;

    const scopeNames = MakeName(0);

    // NOTE(joe/ben Aug 2023): This is a global that is referred to and reset on each call to
    // resolve scope.
    let errors : Array<CS.CompileError>;

    function desugarToplevelTypes(stmts : Array<A.Expr>) : Array<A.Expr> {
      const typeBinds : A.TypeLetBind[] = [];
      const ansStmts : A.Expr[] = [];
      stmts.forEach(s => {
        switch(s.$name) {
          case 's-type': {
            ansStmts.push(s);
            break;
          }
          case 's-newtype': {
            typeBinds.push(A['s-newtype-bind'].app(s.dict.l, s.dict.name, s.dict.namet));
            break;
          }
          case 's-data': {
            const { l, name, params, mixins, variants, "shared-members": shared, "_check-loc": checkLoc, _check } = s.dict;
            const namet = scopeNames.makeAtom(name);
            typeBinds.push(A['s-newtype-bind'].app(l, A['s-name'].app(l, name), namet));
            ansStmts.push(A['s-data-expr'].app(l, name, namet, params, mixins, variants, shared, checkLoc, _check));
            break;
          }
          default: {
            ansStmts.push(s);
          }
        }
      });
      if(typeBinds.length === 0) { return stmts; }
      else {
        return [
          A['s-type-let-expr'].app(
              typeBinds[0].dict.l,
              runtime.ffi.makeList(typeBinds),
              A['s-block'].app(typeBinds[0].dict.l, runtime.ffi.makeList(ansStmts)),
              ansStmts.length > 1)
          ];
      }
    }

    type Contract = TJ.Variant<A.Expr, 's-contract'>;

    type BindingGroup =
      | [ 'let-binds', Contract[], A.LetBind[] ]
      | [ 'letrec-binds', Contract[], A.LetrecBind[] ]
      | [ 'type-let-binds', [], A.TypeLetBind[] ]
    
    function weaveContracts(contracts : Contract[], binds : A.LetBind[]) : A.LetBind[];
    function weaveContracts(contracts : Contract[], binds : A.LetrecBind[]) : A.LetrecBind[];
    function weaveContracts(contracts : Contract[], binds : (A.LetBind[] | A.LetrecBind[])) : (A.LetBind | A.LetrecBind)[];
    function weaveContracts(contracts : Contract[], binds : (A.LetBind[] | A.LetrecBind[])) : (A.LetBind | A.LetrecBind)[] {
      const contractsSD : Map<string, Contract> = new Map();
      contracts.forEach(c => {
        const name = tj.nameToName(c.dict.name);
        if(contractsSD.has(name)) {
          errors.unshift(CS['contract-redefined'].app(c.dict.l, name, contractsSD.get(name)!.dict.l))
        }
        else {
          contractsSD.set(name, c);
        }
      });
      function rebuildBind(b : A.LetBind, newB : A.Bind, newV : A.Expr) : A.LetBind;
      function rebuildBind(b : A.LetrecBind, newB : A.Bind, newV : A.Expr) : A.LetrecBind;
      function rebuildBind(b : A.LetBind | A.LetrecBind, newB : A.Bind, newV : A.Expr) : A.LetBind | A.LetrecBind;
      function rebuildBind(b : A.LetBind | A.LetrecBind, newB : A.Bind, newV : A.Expr) : A.LetBind | A.LetrecBind {
        switch(b.$name) {
          case 's-let-bind': { return A['s-let-bind'].app(b.dict.l, newB, newV); }
          case 's-var-bind': { return A['s-var-bind'].app(b.dict.l, newB, newV); }
          case 's-letrec-bind': { return A['s-letrec-bind'].app(b.dict.l, newB, newV); }
        }
      }
      type SBind = TJ.Variant<A.Bind, 's-bind'>;
      function namesMatch(funargs : List<SBind>, annargs : List<SBind>) {
        const funargsArray = listToArray(funargs);
        const annargsArray = listToArray(annargs);
        if(funargsArray.length !== annargsArray.length) { return false; }
        for(let i = 0; i < funargsArray.length; i += 1) {
          if(tj.nameToName(funargsArray[i].dict.id) !== tj.nameToName(annargsArray[i].dict.id)) { return false; }
        }
        return true;
      }
      function paramsMatch(funparams : List<A.Name>, annparams : List<A.Name>) {
        const funparamsArray = listToArray(funparams);
        const annparamsArray = listToArray(annparams);
        if(funparamsArray.length !== annparamsArray.length) { return false; }
        for(let i = 0; i < funparamsArray.length; i += 1) {
          if(tj.nameToName(funparamsArray[i]) !== tj.nameToName(annparamsArray[i])) { return false; }
        }
        return true;
      }
      function funToLam(bind : A.LetBind) : A.LetBind;
      function funToLam(bind : A.LetrecBind) : A.LetrecBind;
      function funToLam(bind : A.LetBind | A.LetrecBind) : A.LetBind | A.LetrecBind;
      function funToLam(bind : A.LetBind | A.LetrecBind) : A.LetBind | A.LetrecBind {
        const { l, b, value } = bind.dict;
        switch(value.$name) {
          case 's-fun': {
            const { name, params, args, ann, doc, body, '_check-loc' : checkLoc, _check : check, blocky } = value.dict;
            const newBody = A['s-lam'].app(l, name, params, args, ann, doc, body, checkLoc, check, blocky);
            return rebuildBind(bind, b, newBody);
          }
          default: {
            return rebuildBind(bind, b, value);
          }
        }
      }
      function isBlankContract(a : A.Ann) : boolean {
        switch(a.$name) {
          case 'a-blank': { return true; }
          case 'a-tuple': { return listToArray(a.dict.fields).every(isBlankContract); }
          default: {
            return false;
          }
        }
      }

      const revAns = binds.map((bind : (A.LetBind | A.LetrecBind)) => {
        switch(bind.dict.b.$name) {
          case 's-bind': {
            const { l, shadows, id, ann } = bind.dict.b.dict;
            const idName = tj.nameToName(id);
            if(!contractsSD.has(idName)) {
              return funToLam(bind); 
            }
            else {
              const c = contractsSD.get(idName)!;
              contractsSD.delete(idName);
              if(ann.$name === 'a-blank') {
                if(!tj.beforeSrcloc(c.dict.l, bind.dict.value.dict.l)) {
                  errors.unshift(CS['contract-bad-loc'].app(c.dict.l, idName, bind.dict.value.dict.l));
                  return funToLam(bind);
                }
                else {
                  switch(bind.dict.value.$name) {
                    case 's-fun': {
                      const { l: lFun, name, params, args, ann, doc, body, '_check-loc' : checkLoc, _check : check, blocky } = bind.dict.value.dict;
                      const bindargs : SBind[] = listToArray(args) as SBind[];
                      if(!(bindargs.every(a => isBlankContract(a.dict.ann)) && (ann.$name === 'a-blank'))) {
                        errors.unshift(CS['contract-redefined'].app(c.dict.l, idName, lFun));
                        return funToLam(bind);
                      }
                      else if(c.dict.ann.$name === 'a-arrow' || c.dict.ann.$name === 'a-arrow-argnames') {
                        let okParams = true;
                        if(params.$name === 'link' && !(paramsMatch(c.dict.params, params))) { 
                          errors.unshift(CS['contract-inconsistent-params'].app(c.dict.l, idName, lFun));
                          okParams = false;
                        }
                        let okArgs = true;
                        if(c.dict.ann.$name === 'a-arrow-argnames') {
                          if(!(namesMatch(args as List<SBind>, c.dict.ann.dict.args as List<SBind>))) {
                            errors.unshift(CS['contract-inconsistent-names'].app(c.dict.l, idName, lFun));
                            okArgs = false;
                          }
                        }
                        else {
                          if(listToArray(args).length !== listToArray(c.dict.ann.dict.args).length) {
                            errors.unshift(CS['contract-inconsistent-names'].app(c.dict.l, idName, lFun));
                            okArgs = false;
                          }
                        }

                        if(okParams && okArgs) {
                          const argAnns = c.dict.ann.$name === 'a-arrow-argnames'
                            ? listToArray(c.dict.ann.dict.args).map(a => a.dict.ann)
                            : listToArray(c.dict.ann.dict.args);
                          const newargs = argAnns.map((ann : A.Ann, i : number) => {
                            const a = bindargs[i];
                            return A['s-bind'].app(a.dict.l, a.dict.shadows, a.dict.id, ann);
                          });
                          const newLam = A['s-lam'].app(lFun, name, c.dict.params, runtime.ffi.makeList(newargs), c.dict.ann.dict.ret, doc, body, checkLoc, check, blocky);
                          return rebuildBind(bind, bind.dict.b, newLam);
                        }
                        else {
                          return funToLam(bind);
                        }

                      }
                      else {
                        errors.unshift(CS['contract-non-function'].app(c.dict.l, idName, lFun, true));
                        return funToLam(bind);
                      }
                      break; // Check to make sure this stays dead code
                    }
                    default : {
                      if(c.dict.ann.$name === 'a-arrow' || c.dict.ann.$name === 'a-arrow-argnames') {
                        errors.unshift(CS['contract-non-function'].app(c.dict.l, idName, bind.dict.value.dict.l, false));
                        return bind;
                      }
                      else {
                        return rebuildBind(bind, A['s-bind'].app(l, shadows, id, c.dict.ann), bind.dict.value);
                      }
                    }
                  }
                }
              }
              else {
                errors.unshift(CS['contract-redefined'].app(c.dict.l, idName, bind.dict.value.dict.l));
                return funToLam(bind);
              }
            }
          }
          default: {
            return bind;
          }
        }
      });
      contractsSD.forEach((c : Contract, name : string) => {
        errors.unshift(CS['contract-unused'].app(c.dict.l, name));
      });
      return revAns.reverse(); // NOTE(Joe/Ben): we think
    }
    
    type DesugarVisitor =
        TJ.Visitor<A.Expr, A.Expr>
      & TJ.Visitor<A.CasesBranch, A.CasesBranch>
      & TJ.Visitor<A.Member, A.Member>
      & TJ.Visitor<A.Bind, A.Bind>
      & TJ.Visitor<A.CasesBind, A.CasesBind>
      & TJ.Visitor<A.Name, A.Name>
      & TJ.Visitor<Option<A.Expr>, Option<A.Expr>>
      & TJ.Visitor<A.Ann, A.Ann>;

    /**
        Treating stmts as a block, resolve scope.
        There should be no blocks left after this stage of the compiler pipeline.
      */
    function desugarScopeBlock(stmts: A.Expr[], bindingGroup : BindingGroup) : A.Expr {
      if(stmts.length === 0) {
        throw new InternalCompilerError("Should not get an empty block in desugarScopeBlock");
      }
      else {
        const [f, ...rest] = stmts;
        switch(f.$name) {
          case 's-type': {
            return addTypeLetBind(bindingGroup, A['s-type-bind'].app(f.dict.l, f.dict.name, f.dict.params, f.dict.ann), rest);
          }
          case 's-contract': {
            const index = rest.findIndex((e : A.Expr) => e.$name !== 's-contract');
            const [ contracts, restStmts ] = index === -1 ? [ [], rest ] : [ rest.slice(0, index), rest.slice(index) ];
            return addContracts(bindingGroup, [ f, ...(contracts as Contract[]) ], restStmts);
          }
          case 's-let': {
            return addLetBind(bindingGroup, A['s-let-bind'].app(f.dict.l, f.dict.name, f.dict.value), rest);
          }
          case 's-var': {
            return addLetBind(bindingGroup, A['s-var-bind'].app(f.dict.l, f.dict.name, f.dict.value), rest);
          }
          case 's-rec': {
            return addLetrecBind(bindingGroup, A['s-letrec-bind'].app(f.dict.l, f.dict.name, f.dict.value), rest);
          }
          case 's-fun': {
            const { l, name, '_check-loc' : checkLoc, _check : check } = f.dict;
            if(check.$name === 'some') {
              rest.unshift(whereAsCheck(l, name, checkLoc, check.dict.value))
            }
            // NOTE(Ben 2017): deliberately keeping this as an s-fun by using f directly below,
            // it'll get turned into an s-lam in weave-contracts
            const lrb = A['s-letrec-bind'].app(l, A['s-bind'].app(l, false, A['s-name'].app(l, name), A['a-blank']), f)
            return addLetrecBind(bindingGroup, lrb, rest);
          }
          case 's-data-expr': {
            const { l, name, variants } = f.dict;
            function b(l : A.Srcloc, id : string) { return A['s-bind'].app(l, false, A['s-name'].app(l, id), A['a-blank']); }
            function bn(l : A.Srcloc, n : A.Name) { return A['s-bind'].app(l, false, n, A['a-blank']); }
            function variantBinds(dataBlobId : A.Expr, variant : A.Variant) : A.LetrecBind[] {
              const { l, name } = variant.dict;
              const checkerName = makeCheckerName(name);
              const getPart = (n) => A['s-dot'].app(l, dataBlobId, n);
              return [
                A['s-letrec-bind'].app(l, b(l, name), getPart(name)),
                A['s-letrec-bind'].app(l, b(l, checkerName), getPart(checkerName)),
              ]
            }
            const blobId = scopeNames.makeAtom("data-blob");
            const bindData = A['s-letrec-bind'].app(l, bn(l, blobId), f);
            const lookupChecker = A['s-dot'].app(l, A['s-id-letrec'].app(l, blobId, true), makeCheckerName(name));
            const bindDataPred = A['s-letrec-bind'].app(l, b(l, makeCheckerName(name)), lookupChecker);
            const allBinds = listToArray(variants).flatMap((v : A.Variant) => variantBinds(A['s-id-letrec'].app(l, blobId, true), v));
            const allBinds2 = [...allBinds, bindDataPred, bindData];
            return addLetrecBinds(bindingGroup, allBinds2, rest);
          }
          case 's-check': {
            const { l } = f.dict;
            function b(l : A.Srcloc) { return A['s-bind'].app(l, false, A['s-underscore'].app(l), A['a-blank']); }
            return addLetrecBind(bindingGroup, A['s-letrec-bind'].app(l, b(l), f), rest);
          }
          default: {
            if(rest.length === 0) {
              return bindWrap(bindingGroup, f);
            }
            else {
              const restStmt = desugarScopeBlock(rest, [ 'let-binds', [], [] ]);
              let restStmts;
              switch(restStmt.$name) {
                case 's-block': {
                  const { l, stmts } = restStmt.dict;
                  restStmts = [f, ...listToArray(stmts) ];
                  break;
                }
                default: {
                  restStmts = [f, restStmt];
                  break;
                }
              }
              return bindWrap(bindingGroup, A['s-block'].app(f.dict.l, runtime.ffi.makeList(restStmts)));
            }
          }
        }
      }
    }

    function makeCheckerName(s : string) { return "is-" + s; }

    function bindWrap(bindingGroup : BindingGroup, e : A.Expr) : A.Expr {
      const [kind, contracts, revBinds] = bindingGroup;
      if(revBinds.length === 0) { 
        contracts.forEach((c : Contract) => errors.unshift(CS['contract-unused'].app(c.dict.l, tj.nameToName(c.dict.name))));
        return e;
      }
      else {
        switch(kind) {
          case 'let-binds': {
            const withContracts = weaveContracts(contracts, revBinds);
            return A['s-let-expr'].app(revBinds[0].dict.l, runtime.ffi.makeList(withContracts), e, false);
          }
          case 'letrec-binds': {
            const withContracts = weaveContracts(contracts, revBinds);
            return A['s-letrec'].app(revBinds[0].dict.l, runtime.ffi.makeList(withContracts), e, false);
          }
          case 'type-let-binds': {
            return A['s-type-let-expr'].app(revBinds[0].dict.l, runtime.ffi.makeList(revBinds.reverse()), e, false);
          }
        }
      }
    }

    function whereAsCheck(l : A.Srcloc, name : string, checkLoc : Option<A.Srcloc>, check : A.Expr) : A.Expr {
      if(checkLoc.$name === 'some') { l = checkLoc.dict.value; }
      return A['s-check'].app(l, runtime.ffi.makeSome(name), check, false);
    }

    function addTypeLetBind(bindingGroup : BindingGroup, bind : A.TypeLetBind, rest : A.Expr[]) : A.Expr {
      const [kind, contracts, revBinds] = bindingGroup;
      switch(kind) {
        case 'type-let-binds': {
          return desugarScopeBlock(rest, [ kind, contracts, [ bind, ...revBinds ] ]);
        }
        default: {
          return bindWrap(bindingGroup, desugarScopeBlock(rest, [ 'type-let-binds', [], [ bind ] ]));
        }
      }
    }

    function addLetBind(bindingGroup : BindingGroup, bind : A.LetBind, rest : A.Expr[]) : A.Expr {
      const [kind, contracts, revBinds] = bindingGroup;
      let lb;
      switch(bind.$name) {
        case 's-let-bind': {
          lb = simplifyLetBind(bind.dict.l, bind.dict.b, bind.dict.value, []);
          break;
        }
        case 's-var-bind': {
          lb = [ bind ];
          break;
        }
      }
      switch(kind) {
        case 'let-binds': {
          return desugarScopeBlock(rest, [ kind, contracts, [...lb, ...revBinds ] ]);
        }
        default: {
          return bindWrap(bindingGroup, desugarScopeBlock(rest, [ 'let-binds', [], lb ]));
        }
      }
    }

    function addLetrecBind(bindingGroup : BindingGroup, bind : A.LetrecBind, rest : A.Expr[]) : A.Expr {
      return addLetrecBinds(bindingGroup, [ bind ], rest);
    }

    function addLetrecBinds(bindingGroup : BindingGroup, binds : A.LetrecBind[], rest : A.Expr[]) : A.Expr {
      const [kind, contracts, revBinds] = bindingGroup;
      switch(kind) {
        case 'letrec-binds': {
          return desugarScopeBlock(rest, [ kind, contracts, [...binds, ...revBinds ] ]);
        }
        default: {
          return bindWrap(bindingGroup, desugarScopeBlock(rest, [ 'letrec-binds', [], binds ]));
        }
      }
    }

    function addContracts(bindingGroup : BindingGroup, contracts : Contract[], stmts : A.Expr[]) : A.Expr {
      if(stmts.length === 0) {
        throw new InternalCompilerError("Impossible: well-formedness prohibits contracts being last in block (at " + tj.formatSrcloc(contracts[0].dict.l, true) + ")");
      }
      const [kind, contracts2, revBinds] = bindingGroup;
      const [first, ...rest] = stmts;
      if(['s-rec', 's-fun', 's-data-expr', 's-check'].includes(first.$name)) {
        if(kind === 'letrec-binds') {
          return desugarScopeBlock(stmts, [ 'letrec-binds', [...contracts, ...contracts2], revBinds ]);
        }
        else {
          return bindWrap(bindingGroup, desugarScopeBlock(stmts, [ 'letrec-binds', contracts, [] ]));
        }
      }
      else {
        if(kind == 'let-binds') {
          return desugarScopeBlock(stmts, [ 'let-binds', [...contracts, ...contracts2], revBinds ]);
        }
        else {
          return bindWrap(bindingGroup, desugarScopeBlock(stmts, [ 'let-binds', contracts, [] ]));
        }
      }
    }

    function simplifyLetBind(l : A.Srcloc, bind : A.Bind, expr : A.Expr, binds : A.LetBind[]) : A.LetBind[] {
      switch(bind.$name) {
        case 's-bind': {
          binds.unshift(A['s-let-bind'].app(l, bind, expr));
          break;
        }
        case 's-tuple-bind': {
          const { l : lb, fields, 'as-name': asName } = bind.dict;
          let boundExpr : A.Expr;
          let binding : A.LetBind;
          switch(asName.$name) {
            case 'none': {
              const name = scopeNames.makeAtom("tup");
              const newFields = listToArray(fields).map((f : A.Bind) => {
                switch(f.$name) {
                  case 's-bind': { return f.dict.ann; }
                  case 's-tuple-bind': { return A['a-blank']; }
                }
              });
              const ann = A['a-tuple'].app(lb, runtime.ffi.makeList(newFields));
              boundExpr = A['s-id'].app(lb, name);
              binding = A['s-let-bind'].app(lb, A['s-bind'].app(lb, false, name, ann), expr);
              break;
            }
            case 'some': {
              const b = (asName.dict.value as TJ.Variant<A.Bind, 's-bind'>);
              let someBinding;
              switch(b.dict.ann.$name) {
                case 'a-blank': {
                  const ann = A['a-tuple'].app(lb, runtime.ffi.makeList(listToArray(fields).map(f => A['a-blank'])));
                  someBinding = A['s-bind'].app(b.dict.l, b.dict.shadows, b.dict.id, ann);
                  break;
                }
                default: {
                  someBinding = b;
                }
              }
              boundExpr = A['s-id'].app(b.dict.l, b.dict.id);
              binding = A['s-let-bind'].app(l, someBinding, expr);
              break;
            }
          }
          binds.unshift(binding);
          listToArray(fields).forEach((f, i : number) => {
            simplifyLetBind(f.dict.l, f, A['s-tuple-get'].app(f.dict.l, boundExpr, i, f.dict.l), binds);
          });
        }
      }
      return binds;
    }

    const desugarScopeVisitor : DesugarVisitor = {
      's-block': function(self, e) {
        const { l, stmts } = e.dict;
        const newStmts = listToArray(stmts).map((s : A.Expr) => tj.map(self, s));
        return desugarScopeBlock(newStmts, [ 'let-binds', [], [] ]);
      },
      's-let-expr': function(self, e) {
        const { l, binds, body, blocky } = e.dict;
        const vBody = tj.map(self, body);
        const bindsArray = listToArray(binds);
        const newBinds = [];
        bindsArray.forEach((b : A.LetBind) => {
          simplifyLetBind(b.dict.l, b.dict.b, b.dict.value, newBinds);
        });
        return A['s-let-expr'].app(l, runtime.ffi.makeList(newBinds), vBody, blocky);
      },
      's-for': function(self : DesugarVisitor, e) {
        const { l, iterator, bindings, ann, body, blocky } = e.dict;
        const vIterator = tj.map(self, iterator);
        const vAnn = tj.map(self, ann);
        const vBody = tj.map(self, body);
        let newBinds : A.ForBind[] = [];
        let newBody = vBody;
        const binds = listToArray(bindings);
        binds.forEach((b : A.ForBind) => {
          const vBind = tj.map(self, b.dict.bind);
          const vValue = tj.map(self, b.dict.value);
          const lbs = simplifyLetBind(b.dict.l, vBind, vValue, []);
          const argBind = lbs[0];
          newBinds.push(A['s-for-bind'].app(b.dict.l, argBind.dict.b, argBind.dict.value));
          if(lbs.length > 1) {
            newBody = A['s-let-expr'].app(b.dict.l, runtime.ffi.makeList(lbs.slice(1)), newBody, false);
          }
        });
        return A['s-for'].app(l, vIterator, runtime.ffi.makeList(newBinds), vAnn, newBody, blocky);
      },
      's-cases-branch': function(self : DesugarVisitor, e) {
        const { l, 'pat-loc': patLoc, name, args, body } = e.dict;
        const vBody = tj.map(self, body);
        let newBinds : A.CasesBind[] = [];
        let newBody = vBody;
        const argsArray = listToArray(args);
        argsArray.forEach((a : A.CasesBind) => {
          const lbs = simplifyLetBind(a.dict.l, tj.map(self, a.dict.bind), A['s-str'].app(a.dict.l, "placeholder-cases"), []);
          const argBind = lbs[0];
          newBinds.push(A['s-cases-bind'].app(a.dict.l, a.dict['field-type'], argBind.dict.b));
          if(lbs.length > 1) {
            newBody = A['s-let-expr'].app(a.dict.l, runtime.ffi.makeList(lbs.slice(1)), newBody, false);
          }
        });
        return A['s-cases-branch'].app(l, patLoc, name, runtime.ffi.makeList(newBinds), newBody);
      },
      's-fun': function(self, e) {
        const { l, name, params, args, ann, doc, body, '_check-loc' : checkLoc, _check : check, blocky } = e.dict;
        return rebuildFun(A['s-fun'], self, l, name, params, args, ann, doc, body, checkLoc, check, blocky);
      },
      's-lam': function(self, e) {
        const { l, name, params, args, ann, doc, body, '_check-loc' : checkLoc, _check : check, blocky } = e.dict;
        return rebuildFun(A['s-lam'], self, l, name, params, args, ann, doc, body, checkLoc, check, blocky);
      },
      's-method-field': function(self, e) {
        const { l, name, params, args, ann, doc, body, '_check-loc' : checkLoc, _check : check, blocky } = e.dict;
        return rebuildFun(A['s-method-field'], self, l, name, params, args, ann, doc, body, checkLoc, check, blocky);
      }
    };


    type FunctionBuilder = typeof A['s-lam' | 's-method-field' | 's-fun'];
    function rebuildFun(rebuild : FunctionBuilder, visitor : DesugarVisitor, l : A.Srcloc, name : string, params : List<A.Name>, args : List<A.Bind>, ann : A.Ann, doc : string, body : A.Expr, checkLoc : Option<A.Srcloc>, check : Option<A.Expr>, blocky : boolean) : any {
      const vParams = listToArray(params).map((p : A.Name) => tj.map(visitor, p));
      const vAnn = tj.map(visitor, ann);
      const vBody = tj.map(visitor, body);
      const vCheck = tj.map(visitor, check);
      const placeholder = A['s-str'].app(l, "placeholder-rebuild");
      let newBinds : A.Bind[] = [];
      let newBody = vBody;
      const argsArray = listToArray(args);
      argsArray.forEach((a : A.Bind) => {
        const lbs = simplifyLetBind(a.dict.l, tj.map(visitor, a), placeholder, []).reverse();
        const argBind = lbs[0];
        newBinds.push(argBind.dict.b);
        if(lbs.length > 1) {
          newBody = A['s-let-expr'].app(a.dict.l, runtime.ffi.makeList(lbs.slice(1)), newBody, false);
        }
      });
      return rebuild.app(l, name, runtime.ffi.makeList(vParams), runtime.ffi.makeList(newBinds), vAnn, doc, newBody, checkLoc, vCheck, blocky);
    }

    /**
       Remove x = e, var x = e, tuple bindings, and fun f(): e end
       and turn them into explicit let and letrec expressions.
       Do this recursively through the whole program.
       Preconditions on prog:
         - well-formed
       Postconditions on prog:
         - contains no s-provide in headers
         - contains no s-let, s-var, s-data, s-tuple-bind
     */
    function desugarScope(program: A.Program, env: CS.CompileEnvironment): CS.ScopeResolution {
      switch(program.$name) {
        case 's-program': {
          const { l, '_use' : _useRaw, '_provide' : _provideRaw, 'provided-types' : provideTypesRaw, provides, imports: importsRaw, block : body} = program.dict;
          const str = function(s : string) { return A['s-str'].app(l, s); }

          let withImports : A.Expr;
          switch(body.$name) {
            case 's-block': {
              const asArray = runtime.ffi.makeList(desugarToplevelTypes(listToArray(body.dict.stmts)))
              withImports = A['s-block'].app(l, asArray);
              break;
            }
            default: {
              const asArray = runtime.ffi.makeList(desugarToplevelTypes([body]))
              withImports = A['s-block'].app(l, asArray);
            }
          }
          const empty = runtime.ffi.makeList([]);

          function transformToplevelLast(l2 : A.Srcloc, last : A.Expr) : A.Expr {
            const checkers = A['s-dot'].app(l2, AU.checkers.app(l2), "results");
            return A['s-module'].app(l2, last, empty, empty, empty, A['s-app'].app(l2, checkers, empty));
          }

          let withProvides : TJ.Variant<A.Expr, 's-block'>;
          switch(withImports.$name) {
            case 's-block': {
              const { l : l2, stmts } = withImports.dict;
              const stmtsArray = listToArray(stmts);
              const stmtsFront = stmtsArray.slice(0, stmtsArray.length - 1);
              const last = stmtsArray[stmtsArray.length - 1];
              switch(last.$name) {
                case 's-type-let-expr': {
                  const { l : l3, binds, body : body2, blocky } = last.dict;
                  const innerLastArray = listToArray((body2 as TJ.Variant<A.Expr, 's-block'>).dict.stmts);
                  const innerFront = innerLastArray.slice(0, innerLastArray.length - 1);
                  const innerLast = innerLastArray[innerLastArray.length - 1];
                  const innerTransformed = [...innerFront, transformToplevelLast(l3, innerLast)];
                  const newTypeLet = A['s-type-let-expr'].app(l3, binds,
                    A['s-block'].app(body2.dict.l, runtime.ffi.makeList(innerTransformed)), true);
                  const newBlock = A['s-block'].app(l2, runtime.ffi.makeList([...stmtsFront, newTypeLet]))
                  withProvides = newBlock;
                  break;
                }
                default: {
                  withProvides = A['s-block'].app(l2, runtime.ffi.makeList([...stmtsFront, transformToplevelLast(l2, last)]))
                  break;
                }
              }
              break;
            }
            default: {
              throw new InternalCompilerError("Impossible");
            }

          }
          errors = [];

          // NOTE(joe/ben Aug 2023): This next line seems unnecessary, but copying from
          // original Pyret code faithfully (it's just recreating the same block)
          const recombined = A['s-block'].app(withProvides.dict.l, withProvides.dict.stmts);
          const visited = tj.map(desugarScopeVisitor, recombined);
          return CS['resolved-scope'].app(
            A['s-program'].app(
              l, _useRaw, _provideRaw, provideTypesRaw, provides, importsRaw, visited
            ),
            runtime.ffi.makeList(errors)
          );
        }
      }
    }
    function resolveNames(program: A.Program, thismoduleURI: string, initialEnv: CS.CompileEnvironment): CS.NameResolution {
      return undefined as any;
    }
    const exports: Exports['dict']['values']['dict'] = {
      'desugar-scope': runtime.makeFunction(desugarScope),
      'resolve-names': runtime.makeFunction(resolveNames)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})