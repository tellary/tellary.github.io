---
title: "Minesweeper: A Haskell Primer"
---

I wrote a bot that plays [Minesweeper](https://www.google.com/search?q=google minesweeper).

<video
  width="100%"
  muted
  autoplay
  loop
  src="/media/minesweeper_bot.mov"/>
<br>

Minesweeper is puzzle game where a player must identify field cells that contain mines and mark them with flags. If the player believes there is no mine in a cell, she opens it. If there is no mine, the opened field indicates the number of mines in adjacent cells. However, if the player opens a field with a mine, the game ends.

I have loved this game since childhood and decided to automate playing it for several reasons. One reason is that I find formalizing the logic of playing the game to be more enjoyable and productive than playing it manually. But more importantly, I want to use this project to demonstrate why Haskell is such a cool programming language. While there are many great Haskell tutorials and books available, such as ["Real\ World\ Haskell"](https://book.realworldhaskell.org/read/) and ["Learn\ You\ a\ Haskell\ for\ Great\ Good!"](http://www.learnyouahaskell.com/), I believe that building projects is the best way to truly appreciate Haskell's beauty and power.

So instead of writing yet another blog post about the theoretical foundations of Haskell or its strong type system, I invite you to join me on a journey of building a fun program together. We'll start by managing Minesweeper's dependencies with [Nix](https://en.wikipedia.org/wiki/Nix_(package_manager)), then move on to capturing and reading web application screenshots using the [WebDriver](https://hackage.haskell.org/package/webdriver) and [JuicyPixels library](https://hackage.haskell.org/package/JuicyPixels). After that, we'll read the Game Model and build a naive Minesweeper algorithm before diving into the State Monad and use it to supercharge our Minesweeper algorithm.

Of course, this plan may change as I write the posts and refine the project. I still work on the project, the actual implementation featured in the screencast above falls somewhere between the naive and supercharged versions of the algorithm. I may also find it necessary to split some of these topics into separate posts if they become too complicated.

Finally, I'd like to remind you to [follow\ me\ on\ LinkedIn](http://www.linkedin.com/comm/mynetwork/discovery-see-all?usecase=PEOPLE_FOLLOWS&followMember=tellary) to stay updated on the latest posts about the Minesweeper bot.
