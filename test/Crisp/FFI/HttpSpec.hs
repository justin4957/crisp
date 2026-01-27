{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Crisp.FFI.HttpSpec
-- Description : Test suite for HTTP FFI bindings
--
-- Tests for HTTP-related external function definitions, parsing,
-- context registration, and type checking.

module Crisp.FFI.HttpSpec (spec) where

import Test.Hspec

import Crisp.FFI.Http
import Crisp.Parser.Parser
import Crisp.Types.Context
import Crisp.Core.Term (Type(..), simpleType)

import Data.Either (isRight)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Helper to check that parsing succeeds
shouldParse :: (Show a, Show b) => Either a b -> Expectation
shouldParse result = result `shouldSatisfy` isRight

spec :: Spec
spec = do
  httpExternalDefinitionTests
  httpExternalParsingTests
  httpExternalContextTests
  httpTypeTests
  httpResponseTests
  httpErrorTests

-- =============================================================================
-- HTTP External Definition Tests
-- =============================================================================

httpExternalDefinitionTests :: Spec
httpExternalDefinitionTests = describe "HTTP external definitions" $ do
  describe "http_get" $ do
    it "has correct name" $ do
      externalInfoName httpGetExternal `shouldBe` "http_get"

    it "has correct module" $ do
      externalInfoModule httpGetExternal `shouldBe` "http"

    it "has correct external function name" $ do
      externalInfoFunction httpGetExternal `shouldBe` "get"

    it "takes url parameter" $ do
      let fnType = externalInfoType httpGetExternal
      case fnType of
        TyPi "url" paramTy _ _ -> paramTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type with url parameter"

    it "takes headers parameter" $ do
      let fnType = externalInfoType httpGetExternal
      case fnType of
        TyPi _ _ _ (TyPi "headers" paramTy _ _) -> paramTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type with headers parameter"

    it "returns String type" $ do
      let fnType = externalInfoType httpGetExternal
      case fnType of
        TyPi _ _ _ (TyPi _ _ _ retTy) -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

  describe "http_post" $ do
    it "has correct name" $ do
      externalInfoName httpPostExternal `shouldBe` "http_post"

    it "has correct module" $ do
      externalInfoModule httpPostExternal `shouldBe` "http"

    it "has correct external function name" $ do
      externalInfoFunction httpPostExternal `shouldBe` "post"

    it "takes url parameter" $ do
      let fnType = externalInfoType httpPostExternal
      case fnType of
        TyPi "url" paramTy _ _ -> paramTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type with url parameter"

    it "takes body parameter" $ do
      let fnType = externalInfoType httpPostExternal
      case fnType of
        TyPi _ _ _ (TyPi "body" paramTy _ _) -> paramTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type with body parameter"

    it "takes headers parameter" $ do
      let fnType = externalInfoType httpPostExternal
      case fnType of
        TyPi _ _ _ (TyPi _ _ _ (TyPi "headers" paramTy _ _)) -> paramTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type with headers parameter"

    it "returns String type" $ do
      let fnType = externalInfoType httpPostExternal
      case fnType of
        TyPi _ _ _ (TyPi _ _ _ (TyPi _ _ _ retTy)) -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

  describe "http_put" $ do
    it "has correct name" $ do
      externalInfoName httpPutExternal `shouldBe` "http_put"

    it "has correct module" $ do
      externalInfoModule httpPutExternal `shouldBe` "http"

    it "has correct external function name" $ do
      externalInfoFunction httpPutExternal `shouldBe` "put"

    it "takes url, body, and headers parameters" $ do
      let fnType = externalInfoType httpPutExternal
      case fnType of
        TyPi "url" _ _ (TyPi "body" _ _ (TyPi "headers" _ _ _)) -> pure ()
        _ -> expectationFailure "Expected function type with url, body, headers parameters"

    it "returns String type" $ do
      let fnType = externalInfoType httpPutExternal
      case fnType of
        TyPi _ _ _ (TyPi _ _ _ (TyPi _ _ _ retTy)) -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

  describe "http_delete" $ do
    it "has correct name" $ do
      externalInfoName httpDeleteExternal `shouldBe` "http_delete"

    it "has correct module" $ do
      externalInfoModule httpDeleteExternal `shouldBe` "http"

    it "has correct external function name" $ do
      externalInfoFunction httpDeleteExternal `shouldBe` "delete"

    it "takes url and headers parameters" $ do
      let fnType = externalInfoType httpDeleteExternal
      case fnType of
        TyPi "url" _ _ (TyPi "headers" _ _ _) -> pure ()
        _ -> expectationFailure "Expected function type with url, headers parameters"

    it "returns String type" $ do
      let fnType = externalInfoType httpDeleteExternal
      case fnType of
        TyPi _ _ _ (TyPi _ _ _ retTy) -> retTy `shouldBe` simpleType "String"
        _ -> expectationFailure "Expected function type"

-- =============================================================================
-- HTTP External Parsing Tests
-- =============================================================================

httpExternalParsingTests :: Spec
httpExternalParsingTests = describe "HTTP external parsing" $ do
  it "parses http_get external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn http_get(url: String, headers: String) -> String = (\"http\", \"get\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses http_post external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn http_post(url: String, body: String, headers: String) -> String = (\"http\", \"post\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses http_put external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn http_put(url: String, body: String, headers: String) -> String = (\"http\", \"put\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses http_delete external definition" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn http_delete(url: String, headers: String) -> String = (\"http\", \"delete\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses http_get call in expression" $ do
    let src = T.unlines
          [ "module Main"
          , "fn fetch_data(url: String) -> String:"
          , "  external(\"http\", \"get\") url \"{}\""
          ]
    shouldParse $ parseModule "test" src

  it "parses http_post call with body" $ do
    let src = T.unlines
          [ "module Main"
          , "fn send_data(url: String, data: String) -> String:"
          , "  external(\"http\", \"post\") url data \"{\\\"Content-Type\\\": \\\"application/json\\\"}\""
          ]
    shouldParse $ parseModule "test" src

  it "parses multiple HTTP external definitions" $ do
    let src = T.unlines
          [ "module Main"
          , "external fn http_get(url: String, headers: String) -> String = (\"http\", \"get\")"
          , "external fn http_post(url: String, body: String, headers: String) -> String = (\"http\", \"post\")"
          ]
    shouldParse $ parseModule "test" src

  it "parses HTTP call in function body" $ do
    let src = T.unlines
          [ "module CourtListener"
          , "external fn http_get(url: String, headers: String) -> String = (\"http\", \"get\")"
          , ""
          , "fn search_opinions(query: String, api_key: String) -> String:"
          , "  http_get query api_key"
          ]
    shouldParse $ parseModule "test" src

-- =============================================================================
-- HTTP External Context Tests
-- =============================================================================

httpExternalContextTests :: Spec
httpExternalContextTests = describe "HTTP external context" $ do
  it "registers http_get in context" $ do
    let ctx = registerExternal httpGetExternal emptyContext
    lookupExternal "http_get" ctx `shouldSatisfy` isJust

  it "registers http_post in context" $ do
    let ctx = registerExternal httpPostExternal emptyContext
    lookupExternal "http_post" ctx `shouldSatisfy` isJust

  it "registers all HTTP externals" $ do
    let ctx = foldr registerExternal emptyContext httpExternals
    lookupExternal "http_get" ctx `shouldSatisfy` isJust
    lookupExternal "http_post" ctx `shouldSatisfy` isJust
    lookupExternal "http_put" ctx `shouldSatisfy` isJust
    lookupExternal "http_delete" ctx `shouldSatisfy` isJust

  it "lookupHttpExternal finds http_get" $ do
    lookupHttpExternal "http_get" `shouldSatisfy` isJust

  it "lookupHttpExternal finds http_post" $ do
    lookupHttpExternal "http_post" `shouldSatisfy` isJust

  it "lookupHttpExternal finds http_put" $ do
    lookupHttpExternal "http_put" `shouldSatisfy` isJust

  it "lookupHttpExternal finds http_delete" $ do
    lookupHttpExternal "http_delete" `shouldSatisfy` isJust

  it "lookupHttpExternal returns Nothing for unknown" $ do
    lookupHttpExternal "http_patch" `shouldBe` Nothing

  it "isHttpExternal returns True for HTTP functions" $ do
    isHttpExternal "http_get" `shouldBe` True
    isHttpExternal "http_post" `shouldBe` True
    isHttpExternal "http_put" `shouldBe` True
    isHttpExternal "http_delete" `shouldBe` True

  it "isHttpExternal returns False for non-HTTP functions" $ do
    isHttpExternal "time_ms" `shouldBe` False
    isHttpExternal "log" `shouldBe` False

  it "allHttpExternals returns all definitions" $ do
    length allHttpExternals `shouldBe` 4

-- =============================================================================
-- HTTP Type Tests
-- =============================================================================

httpTypeTests :: Spec
httpTypeTests = describe "HTTP types" $ do
  it "urlType is String" $ do
    urlType `shouldBe` simpleType "String"

  it "headersType is String" $ do
    headersType `shouldBe` simpleType "String"

  it "bodyType is String" $ do
    bodyType `shouldBe` simpleType "String"

  it "httpResponseType is String" $ do
    httpResponseType `shouldBe` simpleType "String"

-- =============================================================================
-- HTTP Response Tests
-- =============================================================================

httpResponseTests :: Spec
httpResponseTests = describe "HttpResponse" $ do
  it "stores status code" $ do
    let response = HttpResponse 200 [] "OK"
    httpResponseStatus response `shouldBe` 200

  it "stores headers" $ do
    let headers = [("Content-Type", "application/json"), ("X-Request-Id", "abc123")]
    let response = HttpResponse 200 headers "{}"
    httpResponseHeaders response `shouldBe` headers

  it "stores body" $ do
    let response = HttpResponse 200 [] "{\"result\": \"success\"}"
    httpResponseBody response `shouldBe` "{\"result\": \"success\"}"

  it "supports equality" $ do
    HttpResponse 200 [] "OK" `shouldBe` HttpResponse 200 [] "OK"
    HttpResponse 200 [] "OK" `shouldNotBe` HttpResponse 404 [] "Not Found"

-- =============================================================================
-- HTTP Error Tests
-- =============================================================================

httpErrorTests :: Spec
httpErrorTests = describe "HttpError" $ do
  it "NetworkError stores message" $ do
    let err = NetworkError "Connection refused"
    case err of
      NetworkError msg -> msg `shouldBe` "Connection refused"
      _ -> expectationFailure "Expected NetworkError"

  it "TimeoutError has no message" $ do
    TimeoutError `shouldBe` TimeoutError

  it "TlsError stores message" $ do
    let err = TlsError "Certificate verification failed"
    case err of
      TlsError msg -> msg `shouldBe` "Certificate verification failed"
      _ -> expectationFailure "Expected TlsError"

  it "InvalidUrl stores URL" $ do
    let err = InvalidUrl "not-a-valid-url"
    case err of
      InvalidUrl url -> url `shouldBe` "not-a-valid-url"
      _ -> expectationFailure "Expected InvalidUrl"

  it "HttpStatusError stores status and message" $ do
    let err = HttpStatusError 404 "Not Found"
    case err of
      HttpStatusError status msg -> do
        status `shouldBe` 404
        msg `shouldBe` "Not Found"
      _ -> expectationFailure "Expected HttpStatusError"

  it "supports equality" $ do
    NetworkError "test" `shouldBe` NetworkError "test"
    NetworkError "test" `shouldNotBe` NetworkError "other"
    TimeoutError `shouldBe` TimeoutError
    TlsError "cert" `shouldBe` TlsError "cert"
