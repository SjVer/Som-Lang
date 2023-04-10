const hljs_language_def = () => ({
  keywords: "from use mod let ext as type is of if then else match switch",
  contains: [
    hljs.COMMENT("---", "---"),
    hljs.COMMENT("--", "$"),
    {
      className: "symbol",
      begin: /(?<=\b(let|ext)\s*)[_a-z]\w*\'*/
    },
    // {
    //   className: "params",
    //   begin: /(?<=\b(let|ext)\s*[_a-z]\w*\'*\s*)[_a-z]\w*\'*/
    // },
    {
      className: "meta",
      begin: /!!(\s*[_a-z]+\s*\.)*\s*[_a-z]+/
    },
    {
      className: "number",
      begin: /\b(?:[0-9]+(?:\.[0-9]+)?|0b[0-1]+|0c[0-7]+|0x[0-9a-fA-F]+)\b/,
    },
    {
      className: "string",
      begin: '"',
      end: '"',
      contains: [
        {
          className: "regexp",
          begin: /\\./,
        },
      ],
    },
    {
      className: "string",
      begin: "'(?:[^'\\\\]|" + escape_seq + ")'",
    },
    {
      className: "type",
      begin: /(\'\w+)|(_*[A-Z]\w*)/
    },
    {
      className: "class",
      begin: /\b_*[a-z]\w*(?=\s*::)/,
    },
    {
      className: "operator",
      begin: /<<|>>|=|\/=|<=|>=|<|>|\&\&|\|\||\*|\/|\+|\-|\%|\.\.|~|(?<!!)!(?!!)/ 
    },
    {
      className: "punctuation",
      begin: /\.|,|:|;/
    },
  ],
});

hljs.registerLanguage("som", hljs_language_def);

let code_nodes = Array.from(document.querySelectorAll("code"))
  // Don't highlight `inline code` blocks in headers.
  .filter(function (node) {
    return !node.parentElement.classList.contains("header");
  });

if (window.ace) {
  // language-som class needs to be removed for editable
  // blocks or highlightjs will capture events
  code_nodes
    .filter(function (node) {
      return node.classList.contains("editable");
    })
    .forEach(function (block) {
      block.classList.remove("language-som");
    });

  code_nodes
    .filter(function (node) {
      return !node.classList.contains("editable");
    })
    .forEach(function (block) {
      hljs.highlightBlock(block);
    });
} else {
  code_nodes.forEach(function (block) {
    hljs.highlightBlock(block);
  });
}

// Adding the hljs class gives code blocks the color css
// even if highlighting doesn't apply
code_nodes.forEach(function (block) {
  block.classList.add("hljs");
});
