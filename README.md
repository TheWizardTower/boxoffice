# boxoffice

![Box Office Logo](box-office.jpg)

# **DISCLAIMER**

This is not meant for production use, this is an educational exercise to get
myself familiar with TVars and testing IO functions via QuickCheck.

# Introduction

This is a Haskell library to count hits to a website. It provides an initLib
method, which returns the TVar the rest of the library functions interface with.
This is best done at the toplevel main of your application.

All of the helper functions take this TVar as an argument.

The TVar holds a list of UTCTime. This list starts out empty, and as you record
hits (via the addCount function), the current time is appended to the list. You
can clear the list via the resetCount function.

You can also get the number of hits with getCount, or the raw list of UTCDates
with showList. You can also get a filtered list via getCountAfter and
getCounterBefore, which take a UTCDate as well as a TVar as an argument.

# Example usage

```haskell
main :: IO ()
main = do
  -- Initialize the library. By which I mean, get the TVar from the helper function. :)
  myTVar <- initLib
  -- Report a hit on my awesome website.
  addCount   myTVar
  -- Get the number of hits.
  count <- getCount  myTVar
  putStrLn count
  -- Reset to zero.
  resetCount myTVar
  -- Get the count again, in a seperate variable.
  resetCount <- getCount  myTVar
  putStrLn resetCount
```

# Issues

* This does not persist any data, anywhere. That's a problem. If you're trying
  to get a handle of what your usage profile is, you have to manually hit every
  webserver and (again) manually aggregate the information. If a server fails,
  the metrics go up in smoke. These are annoying in a normal, 9-5 situation, but
  they become utterly untenable if there is service quality degradation.
* Really, this should persist to an external database, or better yet, a service
  like Prometheus, Graphana, or ElasticSearch.
* Another issue with this design is, everybody has to stand in line to report a
  hit, because of how TVars work. You can probably get around this by calling
  addCount asynchronously, but it's still obnoxious.

