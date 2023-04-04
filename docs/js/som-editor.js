var playgrounds = Array.from(
  document.querySelectorAll("pre code.language-som")
).map((el) => el.parentElement);
playgrounds.forEach((block) => update_playground(block));

function update_playground(playground_block) {
  // and install on change listener to dynamically update ACE editors
  if (window.ace) {
    let code_block = playground_block.querySelector("code");
    if (code_block.classList.contains("editable")) {
      let editor = window.ace.edit(code_block);

      // add Ctrl-Enter command to execute rust code
      editor.commands.addCommand({
        name: "run",
        bindKey: {
          win: "Ctrl-Enter",
          mac: "Ctrl-Enter",
        },
        exec: (_editor) => run_som_code(playground_block),
      });
    }
  }
}

function run_som_code(code_block) {
  var result_block = code_block.querySelector(".result");
  if (!result_block) {
    result_block = document.createElement("code");
    result_block.className = "result hljs language-bash";

    code_block.append(result_block);
  }

  let text = playground_text(code_block);

  // var params = {
  //   version: "stable",
  //   optimize: "0",
  //   code: text,
  //   edition: edition,
  // };

  // result_block.innerText = "Running...";
  result_block.innerText = "Playground evaluation is not implemented yet.";

  // fetch_with_timeout("https://play.rust-lang.org/evaluate.json", {
  //   headers: {
  //     "Content-Type": "application/json",
  //   },
  //   method: "POST",
  //   mode: "cors",
  //   body: JSON.stringify(params),
  // })
  //   .then((response) => response.json())
  //   .then((response) => {
  //     if (response.result.trim() === "") {
  //       result_block.innerText = "No output";
  //       result_block.classList.add("result-no-output");
  //     } else {
  //       result_block.innerText = response.result;
  //       result_block.classList.remove("result-no-output");
  //     }
  //   })
  //   .catch(
  //     (error) =>
  //       (result_block.innerText =
  //         "Playground Communication: " + error.message)
  //   );
}

// add buttons div and filename labels
playgrounds.forEach(function (pre_block) {
  var buttons = pre_block.querySelector(".buttons");
  if(!buttons) {
    buttons = document.createElement("div");
    buttons.className = "buttons";
    pre_block.insertBefore(buttons, pre_block.firstChild);
  }
});

// Process playground code blocks
playgrounds.forEach(function (pre_block) {
  if(!pre_block.querySelector("code").classList.contains("editable")) {
    return;
  }

  var buttons = pre_block.querySelector(".buttons");

  // Add play button
  var runCodeButton = document.createElement("button");
  runCodeButton.className = "fa fa-play play-button";
  runCodeButton.hidden = true;
  runCodeButton.title = "Run this code";
  runCodeButton.setAttribute("aria-label", runCodeButton.title);

  buttons.insertBefore(runCodeButton, buttons.firstChild);
  runCodeButton.addEventListener("click", function (e) {
    run_som_code(pre_block);
  });
  
  // Add undo button
  let code_block = pre_block.querySelector("code");
  if (window.ace && code_block.classList.contains("editable")) {
    var undoChangesButton = document.createElement("button");
    undoChangesButton.className = "fa fa-history reset-button";
    undoChangesButton.title = "Undo changes";
    undoChangesButton.setAttribute("aria-label", undoChangesButton.title);

    buttons.insertBefore(undoChangesButton, buttons.firstChild);

    undoChangesButton.addEventListener("click", function () {
      let editor = window.ace.edit(code_block);
      editor.setValue(editor.originalCode);
      editor.clearSelection();
    });
  }
});
