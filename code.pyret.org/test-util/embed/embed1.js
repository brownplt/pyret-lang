window.messages = [];
window.addEventListener('message', function(message) {
  if(message.data.protocol !== 'pyret') { return; }
  messages.push(message);
  const elt = document.createElement("li");
  elt.innerText = JSON.stringify(message.data);
  const messageDisplay = document.getElementById("messages");
  messageDisplay.appendChild(elt);
  elt.scrollIntoView();
  console.log("Receiving message: ", message);
});

window.addEventListener('load', function() {
  const frame = document.createElement("iframe");
  frame.id = "embed1";
  const compilerQuery = window.EMBED_COMPILER ? `?compiler=${window.EMBED_COMPILER}` : "";
  frame.src = `${window.BASE_URL}/editor${compilerQuery}#controlled=true`;
  frame.style = "width: 100%; height: 100%";
  const container = document.getElementById("container");
  container.appendChild(frame);
  window.embedAPI = makeEmbedAPI(frame);
});

