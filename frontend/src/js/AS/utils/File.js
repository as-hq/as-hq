/* @flow */

import type {
  Callback,
} from '../../types/Base';

const FileUtils = {
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
  },

  promptOpen(cb: Callback<Array<File>>) {
    const input: any = document.createElement("input");
    document.body.appendChild(input);
    input.setAttribute("type", "file");
    input.click();
    input.addEventListener("change", () => {
      const fs = [];
      for (var i=0; i<input.files.length; i++) {
        fs.push(input.files[i]);
      }
      cb(fs);
    });
  },
};

export default FileUtils;
