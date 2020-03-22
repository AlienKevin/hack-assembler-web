import * as wasm from "hack-assembler-web";
import "./elm.js";

var app = Elm.Main.init({ node: document.querySelector('main') });
app.ports.sendInput.subscribe(function(source) {
  const rawOutput = wasm.assemble(source);
  const output = (
    rawOutput.startsWith("Ok:")
    ? {
      code: rawOutput.substring("Ok:".length),
      errorMessage: null
    }
    : (
      rawOutput.startsWith("Err:")
      ? {
        code: null,
        errorMessage: rawOutput.substring("Err:".length)
      }
      : {
        code: rawOutput,
        errorMessage: null
      }
    )
  );
  app.ports.receiveOutput.send(output);
});
