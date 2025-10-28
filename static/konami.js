const konami = ["ArrowUp", "ArrowUp", "ArrowDown", "ArrowDown", "ArrowLeft", "ArrowRight", "ArrowLeft", "ArrowRight", "b", "a"];
let current = [];

function arrayEquals(a, b) {
  return Array.isArray(a) &&
    Array.isArray(b) &&
    a.length === b.length &&
    a.every((val, index) => val === b[index]);
}

document.addEventListener("DOMContentLoaded", function() {
  document.addEventListener("keydown", function(event) {
    current.push(event.key);
    if (current.length > konami.length) current.shift();
    if (arrayEquals(current, konami)) {
      document.getElementById("settings").showModal();
    }
  }, false);
});
