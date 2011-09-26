{-# LANGUAGE OverloadedStrings,QuasiQuotes#-}
module Yesod.RepWidget where
import Yesod.Core
import Yesod.Json
--import Control.Applicative
import Text.Hamlet(hamlet)
typeWidget :: ContentType
typeWidget = "html/widget; charset=utf-8"

data RepWidget = RepWidget Content
instance HasReps RepWidget where
    chooseRep (RepWidget widget ) _ = return (typeWidget, widget)

data RepHtmlWidgetJson = RepHtmlWidgetJson Content Content Content
instance HasReps RepHtmlWidgetJson where
    chooseRep (RepHtmlWidgetJson html widget json) = chooseRep
        [ (typeHtml, html) 
        , (typeWidget, widget)
        , (typeJson, json)
        ]
data RepJsonWidget = RepJsonWidget Content Content
instance HasReps RepJsonWidget where
    chooseRep (RepJsonWidget json widget) = chooseRep
        [ (typeJson, json)
        , (typeWidget, widget) 
        ]

data RepHtmlWidget = RepHtmlWidget Content Content
instance HasReps RepHtmlWidget where
    chooseRep (RepHtmlWidget html widget) = chooseRep
        [ (typeHtml, html)
        , (typeWidget, widget) 
        ]
        
--hamletToRepWidget = liftM RepHtml . hamletToContent
                              
class Yesod a => YesodRepWidget a where
  defaultWidgetLayout :: GWidget sub a () -> GHandler sub a RepWidget
  defaultWidgetLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        fmap RepWidget  . hamletToContent $ [hamlet|
        <div .widgetTitle> {pageTitle p}
        <div .widgethHead>^{pageHead p}
        $maybe msg <- mmsg
          <div .widgetMessage>#{msg}
        <div .widgethHead>^{pageHead p} 
        <div .widgetBody>^{pageBody p} 
|]
defaultLayoutHtmlWidgetJson :: YesodRepWidget master
                  => GWidget sub master ()
                  -> Json
                  -> GHandler sub master RepHtmlWidgetJson
defaultLayoutHtmlWidgetJson w json = do
    RepHtml html <- defaultLayout w
    RepWidget widget <- defaultWidgetLayout w
    return $ RepHtmlWidgetJson html widget (toContent json)
defaultLayoutJsonWidget :: YesodRepWidget master
                  => GWidget sub master ()
                  -> Json
                  -> GHandler sub master RepJsonWidget

defaultLayoutJsonWidget w json = do
    RepWidget html' <- defaultWidgetLayout w
    return $ RepJsonWidget (toContent json) html'
defaultLayoutHtmlWidget :: YesodRepWidget master
                  => GWidget sub master ()
                  -> GHandler sub master RepHtmlWidget
defaultLayoutHtmlWidget w = do
    RepHtml html <- defaultLayout w
    RepWidget widget <- defaultWidgetLayout w
    return $ RepHtmlWidget html widget
