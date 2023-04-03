const escape_seq = "\\\\(?:a|b|f|n|r|t|v|\\\\|\\'|\\\"|0)";

const syntax_rules = {
  start: [
    {
      token: "keyword.import.som",
      regex: "\\b(?:use|from|mod)\\b",
    },
    {
      token: "storage.type.integer.som",
      regex: "\\$i.(?:(?:s|u).(?:s|128|64|32|16|8|1)|\\*)",
    },
    {
      token: "storage.type.float.som",
      regex: "\\$f\\.(?:64|32|16|\\*)",
    },
    {
      token: "storage.type.void.som",
      regex: "\\$V",
    },
    {
      token: "constant.numeric.som",
      regex: "\\b(?:[0-9]+(?:\\.[0-9]+)?|0b[0-1]+|0c[0-7]+|0x[0-9a-fA-F]+)\\b",
    },
    {
      token: "string.quoted.single.som",
      regex: "'(?:[^'\\\\]|" + escape_seq + ")'",
    },
    {
      token: "string.quoted.double.som",
      regex: '"',
      push: [
        {
          token: "string.quoted.double.som",
          regex: '"',
          next: "pop",
        },
        {
          token: "constant.character.escape.som",
          regex: escape_seq,
        },
        {
          defaultToken: "string.quoted.doublesom",
        },
      ],
    },
    {
      token: "keyword.definition.som",
      regex: "\\b(?:let|type|ext|is|of)\\b",
    },
    {
      token: "entity.name.namespace.som",
      regex: "\\b_*[a-z][a-zA-Z0-9_]*(?=\\s*\\:\\:)",
    },
    {
      token: "constant.language.builtin",
      regex: "\\#_*[a-z][a-zA-Z0-9_]*",
    },
    {
      token: "entity.name.type.som",
      regex: "\\b_*[A-Z][a-zA-Z0-9_]*'*",
    },
    {
      token: "comment.start.block.som",
      regex: "---",
      stateName: "comment",
      push: [
        {
          token: "comment.end.block.som",
          regex: "---",
          next: "pop",
        },
        {
          defaultToken: "comment.block.som",
        },
      ],
    },
    {
      token: "comment.line.som",
      regex: "--.*$",
    },
    {
      token: "keyword.operator",
      regex: /\$|[-=]>|[-+%^=!&|<>]=?|[*/](?![*/])=?/,
    },
    {
      token: "punctuation.operator",
      regex: /[?:,;.]/,
    },
    {
      token: "paren.lparen",
      regex: /[\[({]/,
    },
    {
      token: "paren.rparen",
      regex: /[\])}]/,
    },
  ],
};

ace.define(
  "ace/mode/som_highlight_rules",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text_highlight_rules",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextHighlightRules =
      require("./text_highlight_rules").TextHighlightRules;

    var SomHighlightRules = function () {
      // regexp must not have capturing parentheses. Use (?:) instead.
      // regexps are ordered -> the first match is used
      this.$rules = syntax_rules;
      this.normalizeRules();
    };

    oop.inherits(SomHighlightRules, TextHighlightRules);

    exports.SomHighlightRules = SomHighlightRules;
  }
);

ace.define(
  "ace/mode/som",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text",
    "ace/mode/som_highlight_rules",
    "ace/mode/folding/cstyle",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    // defines the parent mode
    var TextMode = require("./text").Mode;
    // var Tokenizer = require("../tokenizer").Tokenizer;
    // var MatchingBraceOutdent = require("./matching_brace_outdent").MatchingBraceOutdent;

    // defines the language specific highlighters and folding rules
    var SomHighlightRules = require("./som_highlight_rules").SomHighlightRules;
    //   var SomFoldMode = require("./folding/som").MyNewFoldMode;

    var Mode = function () {
      // set everything up
      this.HighlightRules = SomHighlightRules;
      //   this.$outdent = new MatchingBraceOutdent();
      // this.foldingRules = new SomFoldMode();
    };
    oop.inherits(Mode, TextMode);

    (function () {
      // configure comment start/end characters
      this.lineCommentStart = "--";
      this.blockComment = { start: "---", end: "---" };
    }.call(Mode.prototype));

    exports.Mode = Mode;
  }
);

window.editors.forEach((editor) => {
  editor.getSession().setMode("ace/mode/som");
  editor.setValue(editor.originalCode.trim(), -1);
  editor.originalCode = editor.getValue();
});