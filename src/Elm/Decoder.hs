{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Elm.Decoder
  ( toElmDecoderRef
  , toElmDecoderRefWith
  , toElmDecoderSource
  , toElmDecoderSourceWith
  , renderDecoder
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasDecoder a where
  render :: a -> RenderM Doc

class HasDecoderRef a where
  renderRef :: a -> RenderM Doc

instance HasDecoder ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ": Decoder" <+> stext name) <$$>
      (fnName <+> "=" <$$> indent 4 ctor)
  render (ElmPrimitive primitive) = renderRef primitive

instance HasDecoderRef ElmDatatype where
  renderRef (ElmDatatype name _) = pure $ "decode" <> stext name
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasDecoder ElmConstructor where
  render = \case
    NamedConstructor name val ->
      case val of
        ElmEmpty -> pure ("decode" <+> stext name)
        ElmRef ref ->
          pure ("decode" <+> stext name <+> "|> custom decode" <> stext ref)
        ElmPrimitiveRef ref -> do
          refDoc <- renderRef ref
          pure ("decode" <+> stext name <+> "|> custom" <+> refDoc)
        v@(Values _ _) -> do
          (_, v') <- renderConstructorArgs 0 v
          pure ("decode" <+> stext name <$$> indent 4 v')
        ElmField _ _ -> error "ElmField inside NamedConstructor"
    RecordConstructor name val ->
      case val of
        ElmEmpty -> error "ElmEmpty in RecordConstructor"
        ElmRef _ -> error "ElmRef in RecordConstructor"
        ElmPrimitiveRef _ -> error "ElmRef in RecordConstructor"
        v@(Values _ _) -> do
          v' <- renderConstructorFields v
          pure ("decode" <+> stext name <$$> indent 4 v')
        ElmField field fieldVal -> do
          v <- renderField field fieldVal
          pure ("decode" <+> stext name <$$> indent 4 v)
    mc@(MultipleConstructors cstrs) -> do
      cstrs' <- mapM renderConstructorMatch cstrs
      pure $
        (if isEnumeration mc then "string" else "field \"tag\" string")
        <$$>
        indent 4
          ("|> andThen" <$$>
            indent 4 (newlineparens ("\\x ->" <$$>
              (indent 4 $ "case x of" <$$>
                (indent 4 $
                  vsep cstrs' <$$>
                  "_ -> fail \"Constructor not matched\"")))))

renderConstructorMatch :: ElmConstructor -> RenderM Doc
renderConstructorMatch = \case
  cstr@(NamedConstructor name _) -> do
    val' <- render cstr
    pure $
      dquotes (stext name) <+> "->" <$$> indent 4 ("field \"contents\"" <$$>
        indent 4 (parens val'))
  cstr@(RecordConstructor name _) -> do
    val' <- render cstr
    pure $
      dquotes (stext name) <+> "->" <$$> indent 4 ("field \"contents\"" <$$>
        indent 4 (parens val'))
  _ -> error "recordConstructorMatch: MultipleConstructors"


renderConstructorFields :: ElmValue -> RenderM Doc
renderConstructorFields val =
  case val of
    Values x y -> do
      dx <- renderConstructorFields x
      dy <- renderConstructorFields y
      pure (dx <$$> dy)
    ElmField field fieldVal -> renderField field fieldVal
    ElmRef _ -> error "ElmRef inside RecordConstructor's Values"
    ElmEmpty -> error "ElmEmpty inside RecordConstructor's Values"
    ElmPrimitiveRef _ -> error "ElmPrimitiveRef inside RecordConstructor's Values"

renderField :: T.Text -> ElmValue -> RenderM Doc
renderField field fieldVal = do
  fieldModifier <- asks fieldLabelModifier

  case fieldVal of
    ElmRef ref -> pure $
      "|> required" <+> dquotes (stext (fieldModifier field)) <+> "decode" <>
        stext ref
    ElmPrimitiveRef ref -> do
      refDoc <- renderRef ref
      pure $
        "|> required" <+> dquotes (stext (fieldModifier field)) <+> refDoc
    ElmEmpty -> error "ElmEmpty inside ElmField"
    Values _ _ -> error "Values inside ElmField"
    ElmField _ _ -> error "ElmField inside ElmField"

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
renderConstructorArgs :: Int -> ElmValue -> RenderM (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  pure (i, "|> index" <+> int i <+> rndrVal)

instance HasDecoder ElmValue where
  render (ElmRef name) = pure $ "decode" <> stext name
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv
  render _ = return "unknown"

instance HasDecoderRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = pure "string"
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt
  renderRef (EDict key value) = do
    require "Dict"
    d <- renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return . parens $ "nullable" <+> dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $
      "map2 (,)" <+> parens ("index 0" <+> dx) <+> parens ("index 1" <+> dy)
  renderRef EUnit = pure $ parens "succeed ()"
  renderRef EDate = pure "decodeDate"
  renderRef EInt = pure "int"
  renderRef EBool = pure "bool"
  renderRef EChar = pure "char"
  renderRef EFloat = pure "float"
  renderRef EString = pure "string"

toElmDecoderRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmDecoderRef
  :: ElmType a
  => a -> T.Text
toElmDecoderRef = toElmDecoderRefWith defaultOptions

toElmDecoderSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDecoderSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmDecoderSource
  :: ElmType a
  => a -> T.Text
toElmDecoderSource = toElmDecoderSourceWith defaultOptions

renderDecoder
  :: ElmType a
  => a -> RenderM ()
renderDecoder x = do
  require "Json.Decode exposing (..)"
  require "Json.Decode.Pipeline exposing (..)"
  collectDeclaration . render . toElmType $ x
