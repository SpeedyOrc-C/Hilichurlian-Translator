module Hilichurlian.Translator.EnGb where

import Hilichurlian.Structure ( Sentence )


class TranslatableENGB where
    toEnGb :: Sentence -> String
