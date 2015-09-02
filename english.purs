data Article = A | An

type BaseNoun = {
  singular :: String,
  plural :: String,
  indefiniteArticle :: Article,
  isProper :: Bool
}

data ModifiedNoun
  = Base BaseNoun
  | Modified Adjective BaseNoun ModifiedNoun

human :: BaseNoun
human = {
  singular: "human",
  plural: "humans",
  indefiniteArticle: A,
  isProper: False
}

simpleBaseNoun :: String -> BaseNoun
simpleBaseNoun n = {
    singular: n,
    plural: if endswith "ss" n then n ++ "es" else n ++ "s",
    indefiniteArticle: if startsWithVowel then An else A
  }
  where startsWithVowel = any (\)

type Adjective = {
  root :: String,
  more :: String,
  most :: String
}

good :: Adjective
good = {
  root: "good",
  more: "better",
  most: "best"
}

bad :: Adjective
bad = {
  root: "bad",
  more: "worse",
  most: "worst"
}

red :: Adjective
red = {
  root: "red",
  more: "redder",
  most: "reddest"
}

beautiful :: Adjective
beautiful = {
  root: "beautiful",
  more: "more beautiful",
  most: "most beautiful"
}

type Verb = {
  simple :: String,
  simpleThirdPerson :: String,
  continuitive :: String,
  past :: String,
  subjunctive :: String
}

simpleVerb :: String -> Verb
simpleVerb v = {
    simple: v,
    simpleThirdPerson: v ++ "s",
    past: past,
    subjunctive: past
    }
  where past = if endswith "e" v then v ++ "d" else v ++ "ed"

go :: Verb
go = {
  simple: "go",
  simpleThirdPerson: "goes",
  continuitive: "going",
  past: "went",
  subjunctive: "gone"
}

like :: Verb
like = simpleVerb "like"

data Sentence
  = Phrase

Singular : String -> Noun
IndefiniteArticle : Noun -> Noun
DefiniteArticle : Noun -> Noun
Because : Clause -> Clause -> Sentence
BecauseOf : Noun -> Clause -> Clause
