{ background, name }:
  { inherit name;
    version = "0";
    description = "automatically order bookmarks by use";

    permissions =
      [ "bookmarks"
        "tabs"
        "webNavigation"
      ];

    background =
      { persistent = true;
        scripts = [ background ];
      };

    manifest_version = 2;
  }
