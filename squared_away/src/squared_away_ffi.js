export function focus(id) {
    const input = document.getElementById(id);
    input.focus();
    const length = input.value.length;
    setTimeout(() => {
        input.setSelectionRange(length, length);
    }, 0);   
}