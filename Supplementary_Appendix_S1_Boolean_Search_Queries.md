# Supplementary Appendix S1

# Boolean Search Queries Used for Corpus Construction

## Overview

The analytical corpus was constructed using a two-stage retrieval
strategy.

First, a comprehensive Hungarian political media corpus was established
within the framework of the **OTKA Political Media Corpus** project.
This corpus was designed to capture the broad spectrum of Hungarian
political discourse between **1 January 2010** and **31 December 2023**
and served as the general sampling frame for the present study.

Second, articles specifically related to the **Russo--Ukrainian War**
were identified within the political corpus using an additional Boolean
query targeting Ukraine-, Russia-, refugee-, and war-related
expressions.

The combination of these two retrieval stages ensured that all analysed
documents belonged simultaneously to the Hungarian political media
sphere and to the substantive topic of the Russo--Ukrainian War.

The complete Boolean search expressions used during corpus construction
are reported below to facilitate full computational reproducibility.

------------------------------------------------------------------------

## A. Base Political Corpus (OTKA Political Media Corpus)

The base corpus was retrieved using the following Boolean query,
developed within the OTKA Political Media Corpus project.

``` text
(abszurdisztán* OR ner* OR államfő* OR állampolgár* OR
"bajszos szar*" OR baloldal* OR "böszme feri*" OR
brüsszel* OR "csányi sándor*" OR demokráci* OR demokrat*
OR dk* OR dobrev* OR döbrögisztán* OR ellenzék*
OR eu* OR "fekete-győr andrás*" OR "felcsúti törpe*"
OR fidesz* OR fidessz* OR fletó* OR fülkeforradalom*
OR gyurcsány* OR gyurcsótány* OR illiberális*
OR "jakab péter*" OR jobbik* OR jobboldal*
OR judapest* OR "káder jános*" OR "karácsony ger*"
OR karigeri* OR kdnp* OR képviselőtestület*
OR kereszténydemokrat* OR kommunist* OR kormány*
OR korrupció* OR "kövér lászló*"
OR "köztársasági elnök*" OR kurcsány*
OR kutyapárt* OR külügyminiszter*
OR "lázár jános*" OR "lehet más a politika*"
OR "lézer jános*" OR libajnai* OR liberális*
OR libernyák* OR lmp*
OR "magyar kétfarkú kutya párt*"
OR "magyar szocialista párt*"
OR maszop* OR mazsihisztán*
OR menekült* OR migráció* OR migráns*
OR miniszter* OR miniszterelnök*
OR minisztérium* OR mkkp* OR mszp*
OR nemzetáruló* OR nemzetellenes*
OR "nemzeti együttműködés*"
OR "novák katalin*" OR o1g*
OR orbán* OR országgyűlés*
OR önkormányzat* OR parlament*
OR párt*
OR "pénztáros lőrinc*"
OR politik*
OR pufajkás*
OR rezsiháború*
OR rogán*
OR semjén*
OR "soros hálózat*"
OR "soros szervezet*"
OR szdsz*
OR szélsőbal*
OR szélsőjobb*
OR szijjártó*
OR szocialista*
OR tagállam*
OR "tiborcz istván*"
OR "tróger tóni*"
OR tüntetés*
OR unió*
OR vakcinaellenes*
OR választás*
OR viktátor*
OR "viktor császár*"
OR "viktor király*"
OR "viktor urunk*"
OR zsidesz*
OR zsidóz*)
```

------------------------------------------------------------------------

## B. Ukraine-related Extension

The analytical corpus examined in this study was obtained by filtering
the OTKA Political Media Corpus using the following Ukraine-specific
Boolean query.

The query was applied to news published between **February 2021** and
**January 2024**.

``` text
ukrajn*
OR ukrán*
OR zelenszkij*
OR putyin
OR orosz-ukrán*
OR Oroszország
```
