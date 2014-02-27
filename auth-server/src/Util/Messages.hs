{-# OverloadedStrings #-}

module Util.Messages where

import Messages.RespAuth
import Messages.RespFacade
import Messages.RqAuth
import Messages.RqFacade

isRqAuth01 :: RqAuth -> Bool
isRqAuth01 (RqAuth01 _ _ _) = True
isRqAuth01 _                = False

isRqAuth02 :: RqAuth -> Bool
isRqAuth02 (RqAuth01 _ _ _) = True
isRqAuth02 _              = False

isRqFacade01 :: RqFacade -> Bool
isRqFacade01 (RqFacade01 _ _) = True
isRqFacade01 _                = False

isRespAuth01 :: RespAuth -> Bool
isRespAuth01 (RespAuth01 _ _) = True
isRespAuth01 _                = False

isRespAuth02 :: RespAuth -> Bool
isRespAuth02 (RespAuth02 _)  = True
isRespAuth02 _               = False

isRespFacade01 :: RespFacade -> Bool
isRespFacade01 (RespFacade01 _ _ _ _) = True
isRespFacade01 _                      = False

