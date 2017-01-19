with import <nixpkgs> { };
haskell.lib.buildStackProject {
   ghc = haskell.packages.ghc801.ghc;
   name = "haskll";
   buildInputs = [ zlib glib openssh git autoreconfHook pcre stack ];
   LANG = "en_US.UTF-8";
}

