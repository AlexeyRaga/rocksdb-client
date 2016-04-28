with (import <nixpkgs> {}); with builtins;

stdenv.mkDerivation {

  name = "myEnv";

  buildInputs = if builtins.getEnv "CI" != "" 
    then [
      haskell.packages.lts-5_9.ghc
    ]
    else [
      rocksdb
      haskell.packages.lts-5_9.ghc
    ];
   

#  STACK_IN_NIX_EXTRA_ARGS
#      = " --extra-lib-dirs=${glpk}/lib" 
#      + " --extra-include-dirs=${glpk}/include" 
#      + " --extra-lib-dirs=${pcre}/lib" 
#      + " --extra-include-dirs=${pcre}/include"
#  ;
}