{- LambdaPass - A password manager using GPGME, JSON and Haskell
   Copyright (C) 2015 Steven Williams

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. -}

module Main where

import Text.LambdaPass.Logic (run) 
import Text.LambdaPass.Argument.Parser

import Options.Applicative

main :: IO ()
main = do
  optParser <- parseOptions
  run =<< execParser (optParser `withInfo` desc)
  where desc = "Password manager written in Haskell, keeping your metadata and passwords encrypted together."
