# For use with Haskell IDE Engine or Haskell Language Server using cabal.
# Copy this file over to 'hie.yaml' to use.

cradle:
    cabal:
        - path: "./src"
          component: "lib:myriad"
        - path: "./app"
          component: "myriad:exe:myriad"
