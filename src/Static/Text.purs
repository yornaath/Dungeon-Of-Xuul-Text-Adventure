module Static.Text where

import Data.String (Pattern(..), Replacement(..), replace)

banner :: String
banner = """  
    ___                                        ___  __  __  __           _ 
   /   \_   _ _ __   __ _  ___  ___  _ __     /___\/ _| \ \/ /   _ _   _| |
  / /\ / | | | '_ \ / _` |/ _ \/ _ \| '_ \   //  // |_   \  / | | | | | | |
 / /_//| |_| | | | | (_| |  __/ (_) | | | | / \_//|  _|  /  \ |_| | |_| | |
/___,'  \__,_|_| |_|\__, |\___|\___/|_| |_| \___/ |_|   /_/\_\__,_|\__,_|_|
                    |___/                                                  

"""

intro :: String -> String
intro name = replace (Pattern "@NAME") (Replacement name) """
   _.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._.-=-._
.-'---      - ---     --     ---   -----   - --       ----  ----   -     ---`-.
 )                                                                           (
(        The wandering hero @NAME has found himeself at the entrance          )
 )       of a dark and forboding cave                                        (
(                                                                             )
 )                                                                           (
(                                                                             )
 )                                                                           (
(                                                                             )
 )                                                                           (
(                                                                             )
 )                                                                           (
(                                                                             )
 )                                                                           (
(___       _       _       _       _       _       _       _       _       ___)
    `-._.-' (___ _) `-._.-' `-._.-' )     ( `-._.-' `-._.-' (__ _ ) `-._.-'
            ( _ __)                (_     _)                (_ ___)
            (__  _)                 `-._.-'                 (___ _)
            `-._.-'                                         `-._.-'

""" 

characterCreationHeader:: String

characterCreationHeader = """
Character Creation
------------------

"""