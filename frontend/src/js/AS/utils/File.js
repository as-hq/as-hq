/* @flow */

let FileUtils = {
  blobToFile(theBlob: Blob, fileName: string): File {
    // #anand casts necessary because types Blob and File don't have an explicit OO relationship,
    // but you turn a Blob into a File by adding two fields. because yavascript.
    let f = ((theBlob : any) : File);
    f.lastModifiedDate = new Date();
    f.name = fileName;
    return f;
  },

  promptSave(f: File) {
    let a = document.createElement("a");
    document.body.appendChild(a);
    a.setAttribute('style', 'display: none');
    let url = window.URL.createObjectURL(f);
    a.href = url;
    a.download = f.name;
    a.click();
    window.URL.revokeObjectURL(url);
  }
};

export default FileUtils;
