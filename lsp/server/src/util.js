"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.toDataArray = exports.mergeAll = exports.merge = exports.matchPath = exports.cstWalk = exports.unpackModule = exports.tokenModifiers = exports.tokenTypes = void 0;
exports.tokenTypes = ['function', 'type', 'typeParameter', 'keyword', 'number', 'variable', 'data', 'variant', 'string', 'property', 'namespace', 'comment'];
exports.tokenModifiers = ['readonly'];
function unpackModule(module) {
    return module.dict["provide-plus-types"].dict.values.dict;
}
exports.unpackModule = unpackModule;
function cstWalk(cst, leaf) {
    const path = [];
    function helper(cst) {
        if ("kids" in cst) {
            path.push(cst);
            cst.kids.forEach((kid) => helper(kid));
            path.pop();
        }
        else {
            leaf(cst, path);
        }
    }
    helper(cst);
}
exports.cstWalk = cstWalk;
function matchPath(tok, path, spec) {
    let npath = path.map((v) => v.name);
    for (const rule of spec) {
        for (const optionString of rule.paths) {
            let option = optionString.split("/");
            let match = true;
            let i = npath.length - 1;
            while (option.length > 0 && i >= 0) {
                let next = option.pop();
                if (next === "*") {
                    next = option.pop();
                    i = npath.lastIndexOf(next !== null && next !== void 0 ? next : "/");
                    if (i < 0) {
                        match = false;
                        break;
                    }
                }
                else if (npath[i] !== next) {
                    match = false;
                    break;
                }
                i--;
            }
            if (match && option.length == 0) {
                return {
                    type: rule.type,
                    modifiers: rule.modifiers,
                    tok: tok
                };
            }
        }
    }
    return null;
}
exports.matchPath = matchPath;
function merge(list1, list2) {
    let i1 = 0;
    let i2 = 0;
    let out = [];
    while (i1 < list1.length && i2 < list2.length) {
        let cmp = list1[i1].tok.pos.startChar - list2[i2].tok.pos.startChar;
        if (cmp < 0) {
            out.push(list1[i1]);
            i1++;
        }
        else {
            out.push(list2[i2]);
            i2++;
            if (cmp == 0) {
                i1++; //don't duplicate tokens, prefer the second list
            }
        }
    }
    if (i1 < list1.length) {
        out = out.concat(list1.slice(i1));
    }
    else if (i2 < list2.length) {
        out = out.concat(list2.slice(i2));
    }
    return out;
}
exports.merge = merge;
function mergeAll(lists) {
    return lists.reduce((prev, curr) => merge(prev, curr));
}
exports.mergeAll = mergeAll;
function toDataArray(toks, allowMultiline) {
    let lastline = 1; // LSP starts from 0, parser starts from 1
    let lastcol = 0;
    return toks.flatMap((stok) => {
        let deltaline;
        let deltacol;
        const pos = stok.tok.pos;
        if (pos.startRow == lastline) {
            deltaline = 0;
            deltacol = pos.startCol - lastcol;
        }
        else {
            deltaline = pos.startRow - lastline;
            deltacol = pos.startCol;
        }
        let type = exports.tokenTypes.indexOf(stok.type);
        let modifiers = 0;
        for (let i = 0; i < exports.tokenModifiers.length; i++) {
            if (stok.modifiers.includes(exports.tokenModifiers[i])) {
                modifiers |= (1 << i);
            }
        }
        if (!allowMultiline && pos.startRow !== pos.endRow) {
            lastline = pos.endRow;
            lastcol = 0;
            return stok.tok.value.split("\n").flatMap((line, i) => {
                if (i == 0) {
                    return [deltaline, deltacol, line.length, type, modifiers];
                }
                else {
                    return [1, 0, line.length, type, modifiers];
                }
            });
        }
        else {
            let length = pos.endCol - pos.startCol;
            // some keyword tokens (e.g. CHECKCOLON) include the colon. 
            // We don't want to highlight it in some places and not in others, so remove it if it is present
            if (stok.type === 'keyword' && stok.tok.value.endsWith(':')) {
                length--;
            }
            lastline = pos.startRow;
            lastcol = pos.startCol;
            return [
                deltaline,
                deltacol,
                length,
                type,
                modifiers
            ];
        }
    });
}
exports.toDataArray = toDataArray;
//# sourceMappingURL=util.js.map