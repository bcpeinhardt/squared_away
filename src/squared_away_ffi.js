export function focus(id) {
    const input = document.getElementById(id);
    input.focus();
    const length = input.value.length;
    setTimeout(() => {
        input.setSelectionRange(length, length);
    }, 0);
}

export function saveFile(content, filename) {
    const blob = new Blob([content], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url); // Clean up the URL object
}

export function uploadFile() {
    const fileInput = document.getElementById('csvupload');
    const file = fileInput.files[0];
    return file.text()
}
