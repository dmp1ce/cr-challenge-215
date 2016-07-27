#!/usr/bin/env runhaskell

{-
Coder Radio Challenge for Episode #215
https://www.reddit.com/r/CoderRadio/comments/4ukt4k/episode_215_coding_challenge/

Run with Docker: docker run -v $PWD:/root -it --rm haskell /root/katy_playlist.hs

Run without Docker: ./katy_playlist.hs

Requirements: 
  - GHC
  - runhaskell
  - ( Other external modules here)
-}

import System.IO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of  ("install_requirements":_)  -> putStrLn "Installing requirements!" 
                _                           -> pure ()


  putStrLn "Katy time!"
