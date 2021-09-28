"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.indent = exports.parse = exports.State = exports.LineState = exports.Indent = void 0;
const util_1 = require("./util");
const initial_operators = new Set([
    "DASH", "PLUS", "TIMES", "SLASH", "LT", "LEQ",
    "GT", "GEQ", "EQUALEQUAL", "NEQ", "DOT", "CARET",
    "SPACESHIP", "EQUALTILDE",
    "IS", "ISEQUALEQUAL", "ISEQUALTILDE", "ISSPACESHIP", "BECAUSE",
    "ISROUGHLY", "ISNOT", "ISNOTEQUALEQUAL", "ISNOTEQUALTILDE", "ISNOTSPACESHIP",
    "SATISFIES", "SATISFIESNOT", "RAISES", "RAISESOTHER",
    "RAISESNOT", "RAISESSATISFIES", "RAISESVIOLATES"
]);
const pyret_openers_closed_by_end = new Set(["FUN", "WHEN", "DO",
    "FOR", "IF", "BLOCK", "LET", "TABLE",
    "LOADTABLE", "SELECT", "EXTEND", "SIEVE", "TRANSFORM", "EXTRACT",
    "ORDER", "REACTOR", "SPY"]);
var pyret_delimiter_type;
(function (pyret_delimiter_type) {
    pyret_delimiter_type[pyret_delimiter_type["NONE"] = 0] = "NONE";
    pyret_delimiter_type[pyret_delimiter_type["OPENING"] = 1] = "OPENING";
    pyret_delimiter_type[pyret_delimiter_type["CLOSING"] = 2] = "CLOSING";
    pyret_delimiter_type[pyret_delimiter_type["SUBKEYWORD"] = 3] = "SUBKEYWORD";
    pyret_delimiter_type[pyret_delimiter_type["OPEN_CONTD"] = 4] = "OPEN_CONTD";
    pyret_delimiter_type[pyret_delimiter_type["CLOSE_CONTD"] = 5] = "CLOSE_CONTD";
    pyret_delimiter_type[pyret_delimiter_type["SUB_CONTD"] = 6] = "SUB_CONTD";
    pyret_delimiter_type[pyret_delimiter_type["FOLD_OPEN_CONTD"] = 7] = "FOLD_OPEN_CONTD";
})(pyret_delimiter_type || (pyret_delimiter_type = {}));
; // Extension of opening keyword (acts like OPEN_CONTD *when folding*)
class Indent {
    constructor(funs = 0, cases = 0, data = 0, shared = 0, trys = 0, except = 0, graph = 0, parens = 0, objects = 0, vars = 0, fields = 0, initial = 0, comments = 0, inString = false) {
        this.fn = funs;
        this.c = cases;
        this.d = data;
        this.s = shared;
        this.t = trys;
        this.e = except;
        this.g = graph;
        this.p = parens;
        this.o = objects;
        this.v = vars;
        this.f = fields;
        this.i = initial;
        this.com = comments;
        this.inString = inString;
    }
    set funs(n) { this.fn = Math.max(n, 0); }
    set cases(n) { this.c = Math.max(n, 0); }
    set data(n) { this.d = Math.max(n, 0); }
    set shared(n) { this.s = Math.max(n, 0); }
    set trys(n) { this.t = Math.max(n, 0); }
    set except(n) { this.e = Math.max(n, 0); }
    set graph(n) { this.g = Math.max(n, 0); }
    set parens(n) { this.p = Math.max(n, 0); }
    set objects(n) { this.o = Math.max(n, 0); }
    set vars(n) { this.v = Math.max(n, 0); }
    set fields(n) { this.f = Math.max(n, 0); }
    set initial(n) { this.i = Math.max(n, 0); }
    set comments(n) { this.com = Math.max(n, 0); }
    get funs() { return this.fn; }
    get cases() { return this.c; }
    get data() { return this.d; }
    get shared() { return this.s; }
    get trys() { return this.t; }
    get except() { return this.e; }
    get graph() { return this.g; }
    get parens() { return this.p; }
    get objects() { return this.o; }
    get vars() { return this.v; }
    get fields() { return this.f; }
    get initial() { return this.i; }
    get comments() { return this.com; }
    toString() {
        return ("Fun " + this.fn + ", Cases " + this.c + ", Data " + this.d + ", Shared " + this.s
            + ", Try " + this.t + ", Except " + this.e + ", Graph " + this.g + ", Parens " + this.p
            + ", Object " + this.o + ", Vars " + this.v + ", Fields " + this.f + ", Initial " + this.i
            + ", Comment depth " + this.com);
    }
    copy() {
        return new Indent(this.fn, this.c, this.d, this.s, this.t, this.e, this.g, this.p, this.o, this.v, this.f, this.i, this.com, this.inString);
    }
    zeroOut() {
        this.fn = this.c = this.d = this.s = this.t = this.e = this.g = this.p = this.o = this.v = this.f = this.i = this.com = 0;
    }
    addSelf(that) {
        this.fn += that.fn;
        this.c += that.c;
        this.d += that.d;
        this.s += that.s;
        this.t += that.t;
        this.e += that.e;
        this.g += that.g;
        this.p += that.p;
        this.o += that.o;
        this.v += that.v;
        this.f += that.f;
        this.i += that.i;
        this.com += that.com;
        return this;
    }
    add(that) { return this.copy().addSelf(that); }
    subSelf(that) {
        this.fn -= that.fn;
        this.c -= that.c;
        this.d -= that.d;
        this.s -= that.s;
        this.t -= that.t;
        that.e -= that.e;
        this.g -= that.g;
        this.p -= that.p;
        this.o -= that.o;
        this.v -= that.v;
        this.f -= that.f;
        this.i -= that.i;
        this.com -= that.com;
        return this;
    }
    sub(that) { return this.copy().subSelf(that); }
}
exports.Indent = Indent;
class LineState {
    constructor(tokens, nestingsAtLineStart, nestingsAtLineEnd, deferedOpened, curOpened, deferedClosed, curClosed, delimType) {
        this.tokens = tokens;
        this.nestingsAtLineStart = nestingsAtLineStart;
        this.nestingsAtLineEnd = nestingsAtLineEnd;
        this.deferedOpened = deferedOpened;
        this.curOpened = curOpened;
        this.deferedClosed = deferedClosed;
        this.curClosed = curClosed;
        this.delimType = delimType;
    }
    copy() {
        return new LineState(this.tokens.concat([]), this.nestingsAtLineStart.copy(), this.nestingsAtLineEnd.copy(), this.deferedOpened.copy(), this.curOpened.copy(), this.deferedClosed.copy(), this.curClosed.copy(), this.delimType);
    }
    print() {
        console.log("LineState is:");
        console.log("  NestingsAtLineStart = " + this.nestingsAtLineStart);
        console.log("  NestingsAtLineEnd = " + this.nestingsAtLineEnd);
        console.log("  DeferedOpened = " + this.deferedOpened);
        console.log("  DeferedClosed = " + this.deferedClosed);
        console.log("  CurOpened = " + this.curOpened);
        console.log("  CurClosed = " + this.curClosed);
        console.log("  Tokens = " + this.tokens);
    }
}
exports.LineState = LineState;
class State {
    constructor(lineState, maybeShorthandLanbda) {
        this.lineState = lineState;
        this.maybeShorthandLambda = maybeShorthandLanbda;
    }
    copy() {
        return new State(this.lineState.copy(), this.maybeShorthandLambda);
    }
    static startState() {
        return new State(new LineState([], new Indent(), new Indent(), new Indent(), new Indent(), new Indent(), new Indent(), pyret_delimiter_type.NONE), false);
    }
}
exports.State = State;
function parse(state, prevToken, curToken, nextToken) {
    let ls = state.lineState;
    // Sometimes we want to pick a delimiter type based on the
    // previous token's type
    var inOpening = ls.delimType === pyret_delimiter_type.OPENING
        || ls.delimType === pyret_delimiter_type.OPEN_CONTD;
    var inSubkw = ls.delimType === pyret_delimiter_type.SUBKEYWORD
        || ls.delimType === pyret_delimiter_type.SUB_CONTD;
    ls.delimType = pyret_delimiter_type.NONE;
    if (prevToken === undefined) {
        ls.nestingsAtLineStart = ls.nestingsAtLineEnd.copy();
    }
    if (ls.nestingsAtLineStart.comments > 0 || ls.curOpened.comments > 0 || ls.deferedOpened.comments > 0) {
        if (curToken.name === "COMMENT-END") {
            if (ls.curOpened.comments > 0)
                ls.curOpened.comments--;
            else if (ls.deferedOpened.comments > 0)
                ls.deferedOpened.comments--;
            else if (prevToken === undefined)
                ls.curClosed.comments++;
            else
                ls.deferedClosed.comments++;
        }
        else if (curToken.name === "COMMENT-START") {
            ls.deferedOpened.comments++;
        }
    }
    else if (curToken.name === "COMMENT-START") {
        ls.deferedOpened.comments++;
    }
    else if (curToken.name === "COMMENT") {
        // nothing to do
    }
    else if (curToken.name === "STRING-START") {
        state.lineState.nestingsAtLineEnd.inString = curToken.pos.startCol;
    }
    else if (curToken.name === "STRING-END") {
        state.lineState.nestingsAtLineEnd.inString = false;
    }
    else if (util_1.hasTop(ls.tokens, "NEEDSOMETHING")) {
        ls.tokens.pop();
        if (util_1.hasTop(ls.tokens, "VAR") && ls.deferedOpened.vars > 0) {
            ls.deferedOpened.vars--;
            ls.tokens.pop();
        }
        parse(state, prevToken, curToken, nextToken); // keep going; haven't processed token yet
    }
    else if (prevToken === undefined && initial_operators.has(curToken.name)) {
        ls.curOpened.initial++;
        ls.deferedClosed.initial++;
    }
    else if (curToken.name === "COLON") {
        if (inOpening)
            ls.delimType = pyret_delimiter_type.OPEN_CONTD;
        else if (inSubkw)
            ls.delimType = pyret_delimiter_type.SUB_CONTD;
        if (util_1.hasTop(ls.tokens, "WANTCOLON")
            || util_1.hasTop(ls.tokens, "WANTCOLONOREQUAL")
            || util_1.hasTop(ls.tokens, "WANTCOLONORBLOCK"))
            ls.tokens.pop();
        else if (util_1.hasTop(ls.tokens, "OBJECT")
            || util_1.hasTop(ls.tokens, "REACTOR")
            || util_1.hasTop(ls.tokens, "SHARED")
            || util_1.hasTop(ls.tokens, "BRACEDEXPR")
            || util_1.hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
            if (util_1.hasTop(ls.tokens, "BRACEDEXPR") || util_1.hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
                ls.tokens.pop();
                ls.tokens.push("OBJECT");
            }
            ls.deferedOpened.fields++;
            ls.tokens.push("FIELD", "NEEDSOMETHING");
        }
    }
    else if (curToken.name === "SEMI") {
        if (util_1.hasTop(ls.tokens, "BRACEDEXPR") || util_1.hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
            ls.tokens.pop();
            ls.tokens.push("TUPLE");
        }
    }
    else if (curToken.name === "COLONCOLON") {
        if (util_1.hasTop(ls.tokens, "OBJECT") || util_1.hasTop(ls.tokens, "SHARED")) {
            ls.deferedOpened.fields++;
            ls.tokens.push("FIELD", "NEEDSOMETHING");
        }
    }
    else if (curToken.name === "COMMA") {
        if (util_1.hasTop(ls.tokens, "FIELD")) {
            ls.tokens.pop();
            if (ls.curOpened.fields > 0)
                ls.curOpened.fields--;
            else if (ls.deferedOpened.fields > 0)
                ls.deferedOpened.fields--;
            else
                ls.deferedClosed.fields++;
        }
    }
    else if (curToken.name === "EQUAL") {
        if (util_1.hasTop(ls.tokens, "WANTCOLONOREQUAL"))
            ls.tokens.pop();
        else {
            while (util_1.hasTop(ls.tokens, "VAR")) {
                ls.tokens.pop();
                ls.curClosed.vars++;
            }
            ls.deferedOpened.vars++;
            ls.tokens.push("VAR", "NEEDSOMETHING");
        }
    }
    else if (curToken.name === "VAR" || curToken.name === "REC") {
        ls.deferedOpened.vars++;
        ls.tokens.push("VAR", "NEEDSOMETHING", "WANTCOLONOREQUAL");
    }
    else if (curToken.name === "FUN" || curToken.name === "METHOD" || curToken.name === "LAM") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("FUN", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    }
    else if (curToken.name === "METHOD") {
        if (util_1.hasTop(ls.tokens, "BRACEDEXPR") || util_1.hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
            ls.tokens.pop();
            ls.tokens.push("OBJECT");
        }
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("FUN", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    }
    else if (curToken.name === "LET" || curToken.name === "LETREC" || curToken.name === "TYPE-LET") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("LET", "WANTCOLONORBLOCK");
    }
    else if (curToken.name === "WHEN") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++; // when indents like functions
        ls.tokens.push("WHEN", "WANTCOLONORBLOCK");
    }
    else if (curToken.name === "DO") {
        if (util_1.hasTop(ls.tokens, "DO")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.tokens.push("WHEN", "WANTCOLON");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "FOR") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++; // for-loops indent like functions
        ls.tokens.push("FOR", "WANTCOLONORBLOCK");
    }
    else if (curToken.name === "CASES") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.cases++;
        ls.tokens.push("CASES", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    }
    else if (curToken.name === "DATA") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.data++;
        ls.tokens.push("DATA", "WANTCOLON", "NEEDSOMETHING");
    }
    else if (curToken.name === "ASK") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.cases++;
        ls.tokens.push("IFCOND", "WANTCOLONORBLOCK");
    }
    else if (curToken.name === "SPY") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("SPY", "WANTCOLON");
    }
    else if (curToken.name === "IF" && (prevToken === null || prevToken === void 0 ? void 0 : prevToken.name) !== "ELSE") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("IF", "WANTCOLONORBLOCK", "NEEDSOMETHING");
    }
    else if (curToken.name === "ELSEIF" || (curToken.name === "ELSE" && (nextToken === null || nextToken === void 0 ? void 0 : nextToken.name) === "IF")) {
        if (util_1.hasTop(ls.tokens, "IF")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.tokens.push("WANTCOLON", "NEEDSOMETHING");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "ELSE" || curToken.name === "ELSECOLON") {
        if (util_1.hasTop(ls.tokens, "IF")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.tokens.push("WANTCOLON");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "ROW") {
        if (util_1.hasTop(ls.tokens, "TABLEROW")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
        else if (util_1.hasTop(ls.tokens, "TABLE")) {
            ls.deferedOpened.funs++;
            ls.tokens.push("TABLEROW");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "SOURCECOLON") {
        if (util_1.hasTop(ls.tokens, "LOADTABLESPEC")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.tokens.push("NEEDSOMETHING");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
        else if (util_1.hasTop(ls.tokens, "LOADTABLE")) {
            ls.deferedOpened.funs++;
            ls.tokens.push("LOADTABLESPEC", "NEEDSOMETHING");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "SANITIZE") {
        if (util_1.hasTop(ls.tokens, "LOADTABLESPEC")) {
            if (ls.curOpened.funs > 0)
                ls.curOpened.funs--;
            else if (ls.deferedOpened.funs > 0)
                ls.deferedOpened.funs--;
            else
                ls.curClosed.funs++;
            ls.deferedOpened.funs++;
            ls.tokens.push("NEEDSOMETHING");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
        else if (util_1.hasTop(ls.tokens, "LOADTABLE")) {
            ls.deferedOpened.funs++;
            ls.tokens.push("LOADTABLESPEC", "NEEDSOMETHING");
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
    }
    else if (curToken.name === "BAR") {
        if (util_1.hasTop(ls.tokens, ["OBJECT", "DATA"]) || util_1.hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
            //ls.curClosed.o++;
            if (util_1.hasTop(ls.tokens, "FIELD")) {
                ls.tokens.pop();
                if (ls.curOpened.fields > 0)
                    ls.curOpened.fields--;
                else if (ls.deferedOpened.fields > 0)
                    ls.deferedOpened.fields--;
                else
                    ls.curClosed.fields++;
            }
            if (util_1.hasTop(ls.tokens, "OBJECT"))
                ls.tokens.pop();
        }
        else if (util_1.hasTop(ls.tokens, "DATA"))
            ls.tokens.push("NEEDSOMETHING");
    }
    else if (curToken.name === "WITH") {
        if (util_1.hasTop(ls.tokens, ["WANTOPENPAREN", "WANTCLOSEPAREN", "DATA"])) {
            ls.tokens.pop();
            ls.tokens.pop();
            ls.tokens.push("OBJECT");
        }
        else if (util_1.hasTop(ls.tokens, ["DATA"])) {
            ls.tokens.push("OBJECT");
        }
    }
    else if (curToken.name === "PROVIDE") {
        ls.tokens.push("PROVIDE");
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.shared++;
    }
    else if (curToken.name === "SHARING") {
        ls.curClosed.data++;
        ls.deferedOpened.shared++;
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        if (util_1.hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
            ls.tokens.pop();
            ls.tokens.pop();
            ls.tokens.pop();
            ls.curClosed.objects++;
            ls.tokens.push("SHARED");
        }
        else if (util_1.hasTop(ls.tokens, ["OBJECT", "DATA"])) {
            ls.tokens.pop();
            ls.tokens.pop();
            //ls.curClosed.o++;
            ls.tokens.push("SHARED");
        }
        else if (util_1.hasTop(ls.tokens, "DATA")) {
            ls.tokens.pop();
            ls.tokens.push("SHARED");
        }
    }
    else if (curToken.name === "WHERE" || (curToken.name === "EXAMPLES" && ls.tokens.length > 0)) {
        ls.delimType = (curToken.name === "WHERE") ? pyret_delimiter_type.SUBKEYWORD
            : pyret_delimiter_type.OPENING;
        if (util_1.hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
            ls.tokens.pop();
            ls.tokens.pop();
            ls.curClosed.objects++;
            ls.curClosed.data++;
            ls.deferedOpened.shared++;
        }
        else if (util_1.hasTop(ls.tokens, ["OBJECT", "DATA"])) {
            ls.tokens.pop();
            // ls.curClosed.o++;
            ls.curClosed.data++;
            ls.deferedOpened.shared++;
        }
        else if (util_1.hasTop(ls.tokens, "DATA")) {
            ls.curClosed.data++;
            ls.deferedOpened.shared++;
        }
        else if (util_1.hasTop(ls.tokens, "FUN")) {
            ls.curClosed.funs++;
            ls.deferedOpened.shared++;
        }
        else if (util_1.hasTop(ls.tokens, "SHARED")) {
            ls.curClosed.shared++;
            ls.deferedOpened.shared++;
        }
        ls.tokens.pop();
        ls.tokens.push("CHECK");
    }
    else if ((curToken.name === "CHECK" || curToken.name === "CHECKCOLON") || ((curToken.name === "EXAMPLES" || curToken.name === "EXAMPLESCOLON") && ls.tokens.length === 0)) {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.shared++;
        ls.tokens.push("CHECK");
        if (curToken.name === "EXAMPLES" || curToken.name === "CHECK")
            ls.tokens.push("WANTCOLON");
    }
    else if (curToken.name === "TRY") { // do we even use this token anymore?
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.trys++;
        ls.tokens.push("TRY", "WANTCOLON");
    }
    else if (curToken.name === "EXCEPT") { // or this one?
        ls.delimType = pyret_delimiter_type.OPENING;
        if (ls.curOpened.trys > 0)
            ls.curOpened.trys--;
        else if (ls.deferedOpened.trys > 0)
            ls.deferedOpened.trys--;
        else
            ls.curClosed.trys++;
        if (util_1.hasTop(ls.tokens, "TRY")) {
            ls.tokens.pop();
            ls.tokens.push("WANTCOLON", "WANTCLOSEPAREN", "WANTOPENPAREN");
        }
    }
    else if (curToken.name === "THENCOLON" || curToken.name === "OTHERWISECOLON") {
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
    }
    else if (curToken.name === "BLOCK") {
        if (util_1.hasTop(ls.tokens, "WANTCOLONORBLOCK")) {
            ls.delimType = pyret_delimiter_type.SUBKEYWORD;
        }
        else {
            ls.deferedOpened.funs++;
            ls.tokens.push("BLOCK");
            ls.delimType = pyret_delimiter_type.OPENING;
        }
    }
    else if (curToken.name === "REACTOR") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("REACTOR", "WANTCOLON");
    }
    else if (curToken.name === "TABLE") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("TABLE");
    }
    else if (curToken.name === "LOAD-TABLE") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("LOADTABLE", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-SELECT") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("SELECT", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-EXTEND") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("EXTEND", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-UPDATE") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("TRANSFORM", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-EXTRACT") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("EXTRACT", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-FILTER") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("SIEVE", "WANTCOLON");
    }
    else if (curToken.name === "TABLE-ORDER") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.funs++;
        ls.tokens.push("ORDER", "WANTCOLON");
    }
    else if (curToken.name === "REF-GRAPH") { //Is this even a token that exists anymore?
        ls.deferedOpened.graph++;
        ls.tokens.push("GRAPH", "WANTCOLON");
    }
    else if (curToken.name === "LBRACK") {
        ls.deferedOpened.objects++;
        ls.tokens.push("ARRAY");
        ls.delimType = pyret_delimiter_type.OPENING;
    }
    else if (curToken.name === "RBRACK") {
        ls.delimType = pyret_delimiter_type.CLOSING;
        if (prevToken === undefined)
            ls.curClosed.objects++;
        else
            ls.deferedClosed.objects++;
        if (util_1.hasTop(ls.tokens, "ARRAY"))
            ls.tokens.pop();
        while (util_1.hasTop(ls.tokens, "VAR")) {
            ls.tokens.pop();
            ls.deferedClosed.vars++;
        }
    }
    else if (curToken.name === "LBRACE") {
        ls.deferedOpened.objects++;
        if (state.maybeShorthandLambda)
            ls.tokens.push("BRACEDEXPR");
        else
            ls.tokens.push("BRACEDEXPR_NOLAMBDA");
        ls.delimType = pyret_delimiter_type.OPENING;
    }
    else if (curToken.name === "RBRACE") {
        ls.delimType = pyret_delimiter_type.CLOSING;
        if (prevToken === undefined)
            ls.curClosed.objects++;
        else
            ls.deferedClosed.objects++;
        if (util_1.hasTop(ls.tokens, "FIELD")) {
            ls.tokens.pop();
            if (ls.curOpened.fields > 0)
                ls.curOpened.fields--;
            else if (ls.deferedOpened.fields > 0)
                ls.deferedOpened.fields--;
            else
                ls.curClosed.fields++;
        }
        if (util_1.hasTop(ls.tokens, "OBJECT")
            || util_1.hasTop(ls.tokens, "BRACEDEXPR")
            || util_1.hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")
            || util_1.hasTop(ls.tokens, "TUPLE")
            || util_1.hasTop(ls.tokens, "SHORTHANDLAMBDA"))
            ls.tokens.pop();
        while (util_1.hasTop(ls.tokens, "VAR")) {
            ls.tokens.pop();
            ls.deferedClosed.vars++;
        }
    }
    else if (curToken.name === "LPAREN" || curToken.name === "PARENNOSPACE" || curToken.name === "PARENSPACE") {
        ls.delimType = pyret_delimiter_type.OPENING;
        ls.deferedOpened.parens++;
        if (util_1.hasTop(ls.tokens, "WANTOPENPAREN")) {
            ls.tokens.pop();
        }
        else if (util_1.hasTop(ls.tokens, "BRACEDEXPR")) {
            ls.tokens.pop();
            ls.tokens.push("SHORTHANDLAMBDA", "WANTCOLONORBLOCK");
        }
        else if (util_1.hasTop(ls.tokens, "OBJECT") || util_1.hasTop(ls.tokens, "SHARED")) {
            ls.tokens.push("FUN", "WANTCOLONORBLOCK");
            ls.deferedOpened.funs++;
        }
        else {
            ls.tokens.push("WANTCLOSEPAREN");
        }
    }
    else if (curToken.name === "RPAREN") {
        ls.delimType = pyret_delimiter_type.CLOSING;
        if (ls.curOpened.parens > 0) {
            ls.curOpened.parens--;
        }
        else if (ls.deferedOpened.parens > 0) {
            ls.deferedOpened.parens--;
        }
        else {
            ls.deferedClosed.parens++;
        }
        if (util_1.hasTop(ls.tokens, "WANTCLOSEPAREN"))
            ls.tokens.pop();
        while (util_1.hasTop(ls.tokens, "VAR")) {
            ls.tokens.pop();
            ls.deferedClosed.vars++;
        }
    }
    else if (curToken.name === "END") {
        ls.delimType = pyret_delimiter_type.CLOSING;
        if (util_1.hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
            /* Handles situations such as
             * data A:
             *   | a with:
             *     b : 2 # <- indents as an object field
             * end
             */
            ls.curClosed.fields++;
            ls.tokens.pop();
            ls.tokens.pop();
        }
        else if (util_1.hasTop(ls.tokens, ["OBJECT", "DATA"])) {
            //ls.curClosed.o++;
            ls.tokens.pop();
        }
        else if (util_1.hasTop(ls.tokens, ["TABLEROW", "TABLE"])
            || util_1.hasTop(ls.tokens, ["LOADTABLESPEC", "LOADTABLE"])) {
            ls.tokens.pop();
            ls.curClosed.objects++;
        }
        var top = util_1.peek(ls.tokens);
        var stillUnclosed = true;
        while (stillUnclosed && ls.tokens.length) {
            // Things that are not counted at all:
            //   wantcolon, wantcolonorequal, needsomething, wantopenparen
            // Things that are counted but not closable by end:
            if (top === "OBJECT" || top === "ARRAY") {
                if (ls.curOpened.objects > 0)
                    ls.curOpened.objects--;
                else if (ls.deferedOpened.objects > 0)
                    ls.deferedOpened.objects--;
                else
                    ls.curClosed.objects++;
            }
            else if (top === "WANTCLOSEPAREN") {
                if (ls.curOpened.parens > 0)
                    ls.curOpened.parens--;
                else if (ls.deferedOpened.parens > 0)
                    ls.deferedOpened.parens--;
                else
                    ls.curClosed.parens++;
            }
            else if (top === "FIELD") {
                if (ls.curOpened.fields > 0)
                    ls.curOpened.fields--;
                else if (ls.deferedOpened.fields > 0)
                    ls.deferedOpened.fields--;
                else
                    ls.curClosed.fields++;
            }
            else if (top === "VAR") {
                if (ls.curOpened.vars > 0)
                    ls.curOpened.vars--;
                else if (ls.deferedOpened.vars > 0)
                    ls.deferedOpened.vars--;
                else
                    ls.curClosed.vars++;
            }
            else if (top === "PROVIDE") {
                if (ls.curOpened.shared > 0)
                    ls.curOpened.shared--;
                else if (ls.deferedOpened.shared > 0)
                    ls.deferedOpened.shared--;
                else
                    ls.curClosed.shared++;
            }
            // Things that are counted, and closable by end:
            else if (pyret_openers_closed_by_end.has(top)) {
                if (ls.curOpened.funs > 0)
                    ls.curOpened.funs--;
                else if (ls.deferedOpened.funs > 0)
                    ls.deferedOpened.funs--;
                else
                    ls.curClosed.funs++;
                stillUnclosed = false;
            }
            else if (top === "CASES" || top === "IFCOND") {
                if (ls.curOpened.cases > 0)
                    ls.curOpened.cases--;
                else if (ls.deferedOpened.cases > 0)
                    ls.deferedOpened.cases--;
                else
                    ls.curClosed.cases++;
                stillUnclosed = false;
            }
            else if (top === "DATA") {
                if (ls.curOpened.data > 0)
                    ls.curOpened.data--;
                else if (ls.deferedOpened.data > 0)
                    ls.deferedOpened.data--;
                else
                    ls.curClosed.data++;
                stillUnclosed = false;
            }
            else if (top === "SHARED" || top === "CHECK") {
                if (ls.curOpened.shared > 0)
                    ls.curOpened.shared--;
                else if (ls.deferedOpened.shared > 0)
                    ls.deferedOpened.shared--;
                else
                    ls.curClosed.shared++;
                stillUnclosed = false;
            }
            else if (top === "TRY") {
                if (ls.curOpened.trys > 0)
                    ls.curOpened.trys--;
                else if (ls.deferedOpened.trys > 0)
                    ls.deferedOpened.trys--;
                else
                    ls.curClosed.trys++;
                stillUnclosed = false;
            }
            else if (top === "EXCEPT") {
                if (ls.curOpened.except > 0)
                    ls.curOpened.except--;
                else if (ls.deferedOpened.except > 0)
                    ls.deferedOpened.except--;
                else
                    ls.curClosed.except++;
                stillUnclosed = false;
            }
            else if (top === "GRAPH") {
                if (ls.curOpened.graph > 0)
                    ls.curOpened.graph--;
                else if (ls.deferedOpened.graph > 0)
                    ls.deferedOpened.graph--;
                else
                    ls.curClosed.graph++;
                stillUnclosed = false;
            }
            ls.tokens.pop();
            top = util_1.peek(ls.tokens);
        }
    }
    else if (curToken.name === "STAR" && util_1.hasTop(ls.tokens, ["PROVIDE"])) {
        ls.deferedClosed.shared++;
        ls.delimType = pyret_delimiter_type.CLOSING;
        ls.tokens.pop();
    }
    if (nextToken === undefined) { // End of line; close out nestings fields
        // console.log("We think we're at an end of line");
        // console.log("LineState is currently");
        // ls.print();
        ls.nestingsAtLineStart.addSelf(ls.curOpened).subSelf(ls.curClosed);
        while (util_1.hasTop(ls.tokens, "VAR")) {
            ls.tokens.pop();
            ls.curClosed.vars++;
        }
        ls.nestingsAtLineEnd.addSelf(ls.curOpened).addSelf(ls.deferedOpened)
            .subSelf(ls.curClosed).subSelf(ls.deferedClosed);
        ls.tokens = ls.tokens.concat([]);
        ls.curOpened.zeroOut();
        ls.deferedOpened.zeroOut();
        ls.curClosed.zeroOut();
        ls.deferedClosed.zeroOut();
    }
    // console.log("LineState is now");
    // ls.print();
}
exports.parse = parse;
const COMMENT = 3;
const BAR = 2;
// Note (Eli): I'm not a huge fan of this solution, but it does work
function indent(fullLine, state, useSpaces, indentUnit) {
    var _a, _b;
    let indentSpec = state.lineState.nestingsAtLineStart;
    let indent = 0;
    let spaces = 0;
    for (let key of ["funs", "cases", "data", "shared", "trys", "except", "graph", "parens", "objects", "vars", "fields", "initial"]) {
        let count = indentSpec[key];
        indent += count;
        if (["cases", "data"].includes(key)) {
            spaces += BAR * count;
        }
    }
    if ((indentSpec.comments > 0) || (indentSpec.inString !== false)) {
        let curSpaces = (_b = (_a = fullLine.match(/\s*/)) === null || _a === void 0 ? void 0 : _a[0]) !== null && _b !== void 0 ? _b : "";
        if (curSpaces.length > 0) { // there's already indentation; don't do anything
            return curSpaces;
        }
        else if (indentSpec.inString !== false) { // use string indent
            spaces = indentSpec.inString;
            indent = 0;
        }
        else { // use comment indent
            spaces += COMMENT * indentSpec.comments;
        }
    }
    else if (/^\s*\|([^#]|$)/.test(fullLine)) {
        spaces -= BAR;
    }
    while (spaces >= indentUnit) {
        spaces -= indentUnit;
        indent++;
    }
    return (useSpaces ? " ".repeat(indent * indentUnit) : "\t".repeat(indent)) + " ".repeat(Math.max(spaces, 0));
}
exports.indent = indent;
