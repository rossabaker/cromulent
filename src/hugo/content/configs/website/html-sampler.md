---
title: HTML element sampler
date: 2023-11-05T23:04:07-0500
description: A sampler of HTML elements used to develop the CSS.
---

This is a sampler of HTML elements used to develop the [CSS]({{< relref
"css" >}}).  Examples are adapted from the [MDN HTML
elements
reference](https://developer.mozilla.org/en-US/docs/Web/HTML/Element),
whose code samples are [dedicated to the public
domain](https://developer.mozilla.org/en-US/docs/MDN/Writing_guidelines/Attrib_copyright_license#code_samples).

## Content sectioning

### Address

<p>Contact the author of this page:</p>

<address>
  <a href="mailto:jim@rock.com">jim@rock.com</a><br />
  <a href="tel:+13115552368">(311) 555-2368</a>
</address>

### Article

<article class="forecast">
  <h1>Weather forecast for Seattle</h1>
  <article class="day-forecast">
    <h2>03 March 2018</h2>
    <p>Rain.</p>
  </article>
  <article class="day-forecast">
    <h2>04 March 2018</h2>
    <p>Periods of rain.</p>
  </article>
  <article class="day-forecast">
    <h2>05 March 2018</h2>
    <p>Heavy rain.</p>
  </article>
</article>

### Aside

<p>
  Salamanders are a group of amphibians with a lizard-like appearance, including short legs and a tail in both larval
  and adult forms.
</p>

<aside>
  <p>The Rough-skinned Newt defends itself with a deadly neurotoxin.</p>
</aside>

<p>
  Several species of salamander inhabit the temperate rainforest of the Pacific Northwest, including the Ensatina, the
  Northwestern Salamander and the Rough-skinned Newt. Most salamanders are nocturnal, and hunt for insects, worms and
  other small creatures.
</p>

### Footer

<article>
  <h1>How to be a wizard</h1>
  <ol>
    <li>Grow a long, majestic beard.</li>
    <li>Wear a tall, pointed hat.</li>
    <li>Have I mentioned the beard?</li>
  </ol>
  <footer>
    <p>© 2018 Gandalf</p>
  </footer>
</article>

### Header

<header>
  <a class="logo" href="#">Cute Puppies Express!</a>
</header>

<article>
  <header>
    <h1>Beagles</h1>
    <time datetime="2014-08-12">08.12.2014</time>
  </header>
  <p>I love beagles <em>so</em> much! Like, really, a lot. They’re adorable and their ears are so, so snuggly soft!</p>
</article>

### Headings

<h4>External morphology</h4>
<h5>Head</h5>
<h6>Mouthparts</h6>
<h5>Thorax</h5>
<h6>Prothorax</h6>
<h6>Pterothorax</h6>

### HGroup

<hgroup>
  <h1>Frankenstein</h1>
  <p>Or: The Modern Prometheus</p>
</hgroup>
<p>
  Victor Frankenstein, a Swiss scientist, has a great ambition: to create intelligent life. But when his creature first
  stirs, he realizes he has made a monster. A monster which, abandoned by his master and shunned by everyone who sees
  it, follows Dr Frankenstein to the very ends of the earth.
</p>

### Main

The site layout puts us in `<main>`, and we can't have two.

### Nav

<nav class="crumbs">
  <ol>
    <li class="crumb"><a href="#">Bikes</a></li>
    <li class="crumb"><a href="#">BMX</a></li>
    <li class="crumb">Jump Bike 3000</li>
  </ol>
</nav>

<h1>Jump Bike 3000</h1>
<p>
  This BMX bike is a solid step into the pro world. It looks as legit as it rides and is built to polish your skills.
</p>

### Section

<h1>Choosing an Apple</h1>
<section>
  <h2>Introduction</h2>
  <p>This document provides a guide to help with the important task of choosing the correct Apple.</p>
</section>

<section>
  <h2>Criteria</h2>
  <p>
    There are many different criteria to be considered when choosing an Apple — size, color, firmness, sweetness,
    tartness...
  </p>
</section>

### Search

<header>
  <h1>Movie website</h1>
  <search>
    <form action="#">
      <label for="movie">Find a Movie</label>
      <input type="search" id="movie" name="q" />
      <button type="submit">Search</button>
    </form>
  </search>
</header>
