window.messages = [];
function displayMessage(message) {
  const elt = document.createElement("li");
  elt.innerText = JSON.stringify(message.data);
  const messageDisplay = document.getElementById("messages");
  messageDisplay.appendChild(elt);
  elt.scrollIntoView();
}
window.addEventListener('message', function(message) {
  if(message.data.protocol !== 'pyret') { return; }
  messages.push(message);
  if(message.data.data.type === 'pyret-init') { return; }
  if(message.source === window.frame1.contentWindow) {
    window.frame2API.postMessage(message.data);
  }
  if(message.source === window.frame2.contentWindow) {
    window.frame1API.postMessage(message.data);
  }
  
});

function addFrame(id) {
  const frame = document.createElement("iframe");
  frame.id = id;
  const compilerQuery = window.EMBED_COMPILER ? `?compiler=${window.EMBED_COMPILER}` : "";
  frame.src = `${window.BASE_URL}/editor${compilerQuery}#controlled=true`;
  frame.style = "width: 100%; height: 49%";
  const container = document.getElementById("container");
  container.appendChild(frame);
  return frame;
}

window.addEventListener('load', function() {
  window.frame1 = addFrame('embed1');
  window.frame2 = addFrame('embed2');
  
  window.frame1API = makeEmbedAPI(frame1);
  window.frame2API = makeEmbedAPI(frame2);
});
