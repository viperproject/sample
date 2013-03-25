meta version "v2.2,nothing";
meta name "TouchDrive";
meta icon "airplane";
meta color "#ff00008b";
//
meta platform "current";

action main(Username: String, Password: String, FileUploads: FileUploadLink_Collection, FileDownloads: FileDownloadLink_Collection) {
  data→isLiveAuthenticated := code→authenticate("", "");
  if data→isLiveAuthenticated then {
    skip;
  }
}

action authenticate(username: String, password: String) returns isAuthenticated: Boolean {
  $isAuthenticated := true;
  meta private;
}
var isLiveAuthenticated : Boolean {
  // True if user ia authenticated to Windows Live, otherwise false
}
table FileUploadLink {
  type = "Object";
  fields {
    LocalFilePath: String_field
    SkyDriveFolderName: Number_field
  }
}
table FileDownloadLink {
  type = "Object";
  fields {
    LocalFolderPath: String_field
    SkyDriveFilePath: String_field
  }
}

