{ nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;

  revealjs = pkgs.fetchFromGitHub {
    owner = "hakimel";
    repo = "reveal.js";
    rev = "43eada79901830702bd40dce857831aef8e76759";
    sha256 = "5be5c1b831e0f4a4f955f76340c2d08c8a1a57c5be5dd68592fd5be511e76bda";
  };

in
  pkgs.stdenv.mkDerivation {
    name = "ylj19-talk";
    src = ./.;

    unpackPhase = ''
      mkdir -p $name/{reveal.js,css,images}
      cd $name
      cp -r ${revealjs}/* ./reveal.js/
      cp $src/css/* ./css/
      cp $src/images/* ./images/
    '';

    buildPhase = ''
      pandoc -t revealjs --template=$src/template.revealjs --variable=codedir:$out --variable=transition:none --highlight-style=zenburn -s $src/slides.md -o index.html
    '';

    installPhase = ''
      mkdir $out
      cp -r ./* $out/
    '';

    phases = ["unpackPhase" "buildPhase" "installPhase"];

    buildInputs = [pkgs.pandoc];
  }
