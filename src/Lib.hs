{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data Candidatura = Candidatura
  { nombre     :: String
  , escaños    :: Int
  , votos      :: Int
  , porcentaje :: Double
  }

$(deriveJSON defaultOptions ''Candidatura)

type API = "congreso" :> Get '[JSON] [Candidatura]

startApp :: IO ()
startApp = run 1234 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = eleccionesServer

eleccionesServer :: Server API
eleccionesServer =
  getCongreso

  where
    getCongreso :: Handler [Candidatura]
    getCongreso = return candidaturasCongreso
















candidaturasCongreso = [
  Candidatura "PSOE"               123 7480755  28.68,
  Candidatura "PP"                  66 4356023  16.7,
  Candidatura "Cs"                  57 4136600  15.86,
  Candidatura "PODEMOS-IU-EQUO"     35 3118191  11.95,
  Candidatura "VOX"                 24 2677173  10.26,
  Candidatura "ERC-SOBIRANISTES"    15 1015355  3.89,
  Candidatura "ECP-GUANYEM EL CANVI" 7 614738  2.36,
  Candidatura "JxCAT-JUNTS"          7 497638 1.91,
  Candidatura "EAJ-PNV"              6 39462  1.517,
  Candidatura "EH Bildu"             4 258840  0.99,
  Candidatura "CCa-PNC"              2 137196  0.53,
  Candidatura "NA+"                  2 107124 0.41,
  Candidatura "COMPROMÍS 2019"       1 172751 0.66,
  Candidatura "PRC"                  1 52197 0.20,
  Candidatura "PACMA"                0 326045  1.25]
