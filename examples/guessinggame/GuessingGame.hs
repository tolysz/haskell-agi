module Main where

-- Standard Haskell Modules

import Control.Concurrent
import Control.Monad.Trans
import Data.Maybe
import Data.Word
import System.IO
import System.Random
import System.Posix.Unistd

-- 3rd Party Modules

import Network.AGI

main :: IO ()
main =
       run mainAGI (undefined) -- Ignore

mainAGI :: AGI ()
mainAGI =
    do answer
       liftIO $ (threadDelay (1 * 10^6))
       playGame
       hangUp Nothing
       return ()

playGame :: AGI ()
playGame =
    do secretIndex <- liftIO $ randomRIO (0, 9)
       let secretDigit = [Zero .. Nine] !! secretIndex
       streamFile "guessing-game-intro" [] Nothing
       mGuess <- waitForDigit (-1)
       let firstGuess =
               case mGuess of
                 (Just (Just digit)) -> digit
                 _ -> Zero
       play (compare secretDigit) firstGuess
       return ()

play :: (Digit -> Ordering) -> Digit -> AGI ()
play oracle guess =
    loop guess
    where
      loop guess =
          case oracle guess of
            LT -> 
                do streamFile "guessing-game-lower" [] Nothing
                   sayDigits [guess] []
                   mNextGuess <- waitForDigit (-1)
                   case mNextGuess of
                     (Just (Just nextGuess)) -> loop nextGuess
                     _ -> loop guess
            GT ->
                do streamFile "guessing-game-higher" [] Nothing
                   sayDigits [guess] []
                   mNextGuess <- waitForDigit (-1)
                   case mNextGuess of
                     (Just (Just nextGuess)) -> loop nextGuess
                     _ -> loop guess
            EQ ->
                do streamFile "guessing-game-yay" [] Nothing
                   sayDigits [guess] []
                   streamFile "guessing-game-correct" [] Nothing
                   return ()

{-

Let's play a game! I am think of a number between 0 and 9. Can you
guess what it is? Enter your guess using the number pad on your phone.

Sorry, the number I am thinking of is higher than

Sorry, the number I am thinking of is less than

Yay! 

is the number I was thinking of! How did you know?

-}