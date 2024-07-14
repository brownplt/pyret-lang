import * as J from 'estree';
import type * as Escodegen from 'escodegen';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as T from './ts-type-structs';
import * as Path from 'path';
import type * as CodegenHelpers from './ts-codegen-helpers';
import type { Variant } from './ts-codegen-helpers';

({
  requires: [{ 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']} ],
  nativeRequires: ['escodegen', 'path'],
  provides: {
    values: {
      'compile-provides': 'tany',
      'compile-provides-override-uri': 'tany',
    }
  },
  theModule: function(runtime, _, __, TCH : CodegenHelpers.Exports, escodegen : (typeof Escodegen), P : (typeof Path)) {
    const {
      ExhaustiveSwitchError,
      Literal,
      ObjectExpression,
      Property,
      compileSrcloc,
      listToArray,
      ArrayExpression,
      nameToKey,
      InternalCompilerError,
      nameToName,
      ConditionalExpression,
    } = TCH;

    // NOTE(alex): Used only by compile mode cm-builtin-stage-1
    //   See compile-provides-override-uri() for more info
    let ORIGIN_URI_OVERRIDE = false

    function builtinSrcloc(uri: string): Variant<A.Srcloc, 'builtin'> {
      return {
        $name: 'builtin',
        dict: { "module-name": uri },
      }
    }

    function compileOrigin(context, bo: CS.BindOrigin): J.ObjectExpression {
      const override = "builtin://" + P.basename(bo.dict['uri-of-definition'], ".arr");
      if (!ORIGIN_URI_OVERRIDE || bo.dict['definition-bind-site'].$name === 'builtin') {
        return ObjectExpression([
          Property("local-bind-site", compileSrcloc(context, bo.dict['local-bind-site'], false)),
          Property("definition-bind-site", compileSrcloc(context, bo.dict['definition-bind-site'], false)),
          Property("new-definition", Literal(bo.dict['new-definition'])),
          Property("uri-of-definition", Literal(bo.dict['uri-of-definition']))
        ]);
      } else {
        return ObjectExpression([
          Property("local-bind-site", compileSrcloc(context, builtinSrcloc(override), false)),
          Property("definition-bind-site", compileSrcloc(context, builtinSrcloc(override), false)),
          Property("new-definition", Literal(bo.dict['new-definition'])),
          Property("uri-of-definition", Literal(override))
        ])
      }
    }

    function compileTypeMember(name, typ : T.Type) {
      return Property(name, compileProvidedType(typ));
    }

    function compileTypeVariant(variant : T.TypeVariant): J.ArrayExpression {
      switch(variant.$name) {
        case 't-variant': {
          const compiledMembers = ArrayExpression(listToArray(variant.dict.fields).map((field) => {
            const [memName, typ] = field.vals;
            switch(typ.$name) {
              case 't-ref':
                return ArrayExpression([Literal("ref"), Literal(memName), compileProvidedType(typ.dict.typ)]);
              default:
                return ArrayExpression([Literal(memName), compileProvidedType(typ)]);
            }
          }));
          const compiledWithMembers = ObjectExpression(variant.dict['with-fields'].$underlyingMap.keys().map((memName) => {
            return compileTypeMember(memName, variant.dict['with-fields'].$underlyingMap.get(memName, null));
          }));
          return ArrayExpression([Literal(variant.dict.name), compiledMembers, compiledWithMembers])
        }
        case 't-singleton-variant': {
          const compiledWithMembers = ObjectExpression(variant.dict['with-fields'].$underlyingMap.keys().map((memName) => {
            return compileTypeMember(memName, variant.dict['with-fields'].$underlyingMap.get(memName, null));
          }));
          return ArrayExpression([Literal(variant.dict.name), compiledWithMembers]);
        }
        default:
          throw new ExhaustiveSwitchError(variant);
      }
    }

    function compileProvidedData(context, de : CS.DataExport): J.ArrayExpression {
      switch(de.$name) {
        case 'd-alias': {
          return ArrayExpression([
            Literal("data-alias"),
            compileOrigin(context, de.dict.origin),
            Literal(de.dict.name),
          ]);
        }
        case 'd-type': {
          const typ = de.dict.typ;
          switch(typ.$name) {
            case 't-data': {
              return ArrayExpression([
                Literal("data"),
                compileOrigin(context, de.dict.origin),
                Literal(typ.dict.name),
                ArrayExpression(listToArray(typ.dict.params).map((p) => {
                  switch(p.$name) {
                    case 't-var': return Literal(nameToKey(p.dict.id));
                    default:
                      throw new InternalCompilerError(`Expected type to have an id field, but was a ${p.$name}`);
                  }
                })),
                ArrayExpression(listToArray(typ.dict.variants).map(compileTypeVariant)),
                ObjectExpression(typ.dict.fields.$underlyingMap.keys().map((memName) => {
                  return compileTypeMember(memName, typ.dict.fields.$underlyingMap.get(memName, null));
                })),
              ]);
            }
            default:
              throw new ExhaustiveSwitchError(typ.$name);
          }
        }
        default:
          throw new ExhaustiveSwitchError(de);
      }
    }

    // NOTE(alex): Used only by compile mode cm-builtin-stage-1
    //   Needed to override origin URIs in order for "include from" syntax to function with values
    //   Used to compile builtin arr modules without messing with URIs before codegen which
    //     MAY or MAY NOT break the compilation pipeline
    function compileProvidesOverrideUri(context, provides : CS.Provides, override : boolean): string {
      const curOverride = ORIGIN_URI_OVERRIDE;
      ORIGIN_URI_OVERRIDE = override;
      const result = compileProvides(context, provides)
      ORIGIN_URI_OVERRIDE = curOverride;
      return result;
    }

    function compileProvidedType(typ : T.Type): J.Expression {
      switch(typ.$name) {
        case 't-name': {
          switch(typ.dict['module-name'].$name) {
            case 'local': {
              return ObjectExpression([
                  Property("tag", Literal("name")),
                  Property("origin", ObjectExpression([Property("import-type", Literal("$ELF"))])),
                  Property("name", Literal(nameToName(typ.dict.id)))]); // TODO: toname or key?
            }
            case 'module-uri': {
              return ObjectExpression([
                  Property("tag", Literal("name")),
                  Property("origin", ObjectExpression([Property("import-type", Literal("uri")), Property("uri", Literal(typ.dict['module-name'].dict.uri))])),
                  Property("name", Literal(nameToName(typ.dict.id)))]); // TODO: toname or key?}
            }
            case 'dependency':
              throw new InternalCompilerError("Dependency-origin names in provided-types shouldn't be possible");
          }
        }
        case 't-var': {
          return ArrayExpression([Literal("tid"), Literal(nameToKey(typ.dict.id))]); // NOTE(joe): changed to .key()
        }
        case 't-arrow': {
          return ArrayExpression(
            [Literal("arrow"),
              ArrayExpression(listToArray(typ.dict.args).map(compileProvidedType)), compileProvidedType(typ.dict.ret)]);
        }
        case 't-app': {
          return ArrayExpression(
            [Literal("tyapp"), compileProvidedType(typ.dict.onto),
              ArrayExpression(listToArray(typ.dict.args).map(compileProvidedType))]);
        }
        case 't-top': return Literal("tany");
        case 't-bot': return Literal("tbot");
        case 't-record': {
          return ArrayExpression(
            [Literal("record"), ObjectExpression(typ.dict.fields.$underlyingMap.keys().map((key) => {
              return compileTypeMember(key, typ.dict.fields.$underlyingMap.get(key, null));
            }))]);
        }
        case 't-tuple': {
          return ArrayExpression(
            [Literal("tuple"), ArrayExpression(listToArray(typ.dict.elts).map(compileProvidedType))]);
        }
        case 't-forall': {
          return ArrayExpression(
            [Literal("forall"),
              ArrayExpression(listToArray(typ.dict.introduces).map((p) => {
                switch(p.$name) {
                  case 't-var': return Literal(nameToKey(p.dict.id));
                  default:
                    throw new InternalCompilerError(`Expected type to have an id field, but was a ${p.$name}`);
                }
              })), compileProvidedType(typ.dict.onto)]);
          }
        // case t-ref(_, _) =>
        // case t-existential(_, _) =>
        case 't-data-refinement': {
          return ArrayExpression(
            [Literal("data%"), compileProvidedType(typ.dict['data-type']), Literal(typ.dict['variant-name'])]);
        }
        default:
          return ConditionalExpression(Literal(false), Literal(String(typ)), Literal("tany"));
      }
    }

    function compileProvides(context, provides: CS.Provides): string {
      switch(provides.$name) {
        case 'provides': {
          const moduleFields = provides.dict.modules.$underlyingMap.keys().map((m) => {
            return Property(m, ObjectExpression([Property("uri", Literal(provides.dict.modules.$underlyingMap.get(m, null)))]));
          });
          const valueFields = provides.dict.values.$underlyingMap.keys().map((v) => {
            const val = provides.dict.values.$underlyingMap.get(v, null);
            switch(val.$name) {
              case 'v-alias':
                return Property(v, ObjectExpression([
                  Property("bind", Literal("alias")),
                  Property("origin", compileOrigin(context, val.dict.origin)),
                  Property("original-name", Literal(val.dict['original-name'])),
                  Property("typ", Literal(false))
                ]));
              case 'v-just-type':
                return Property(v, ObjectExpression([
                  Property("bind", Literal("let")),
                  Property("origin", compileOrigin(context, val.dict.origin)),
                  Property("typ", compileProvidedType(val.dict.t))
                ]))
              case 'v-var':
                return Property(v, ObjectExpression([
                  Property("bind", Literal("var")),
                  Property("origin", compileOrigin(context, val.dict.origin)),
                  Property("typ", compileProvidedType(val.dict.t))
                ]))
              case 'v-fun': {
                let flatness: number | boolean;
                switch(val.dict.flatness.$name) {
                  case 'none': flatness = false; break;
                  case 'some': flatness = val.dict.flatness.dict.value; break;
                }
                return Property(v, ObjectExpression([
                  Property("bind", Literal("fun")),
                  Property("origin", compileOrigin(context, val.dict.origin)),
                  Property("flatness", Literal(flatness)),
                  Property("name", Literal(val.dict.name)),
                  Property("typ", compileProvidedType(val.dict.t))
                ]))
              }
              default:
                throw new ExhaustiveSwitchError(val);  
            }
          });
          const dataFields = provides.dict['data-definitions'].$underlyingMap.keys().map((d) => {
            return Property(d, compileProvidedData(context, provides.dict['data-definitions'].$underlyingMap.get(d, null)));
          });
          const aliasFields = provides.dict.aliases.$underlyingMap.keys().map((a) => {
            return Property(a, compileProvidedType(provides.dict.aliases.$underlyingMap.get(a, null)))
          });
          const ans = ObjectExpression([
            Property("modules", ObjectExpression(moduleFields)),
            Property("values", ObjectExpression(valueFields)),
            Property("datatypes", ObjectExpression(dataFields)),
            Property("aliases", ObjectExpression(aliasFields)),
          ]);
          return escodegen.generate(ans, {
            format: {
              json: true
            }
          });
        }
      }
    }

    return runtime.makeJSModuleReturn({
      compileProvides,
      compileProvidesOverrideUri,
    });
  }
})

export interface Exports {
  compileProvides: (context, provides: CS.Provides) => string,
  compileProvidesOverrideUri: (context, provides: CS.Provides, override: boolean) => string,
}