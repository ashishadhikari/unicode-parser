module WarnaHelper where

import Warna

[a, aa, i, ii, u, uu, r, e, ai, o, au] = map Vowel "अआइईउऊऋएऐओऔ"

[ka, kha, ga, gha, nGa] = map Consonant "कखगघङ"
[ca, cha, ja, jha, nya] = map Consonant "चछजझञ"
[tTa, tTha, dDa, dDha, nNa] = map Consonant "टठडढण"
[ta, tha, da, dha, na] = map Consonant "तथदधन"
[pa, pha, ba, bha, ma] = map Consonant "पफबभम"
[ya, ra, la, wa] = map Consonant "यरलव"
[sSha, sha, sa, ha] = map Consonant "शषसह"

candrawindu = PostVowelMarker 'ँ'
anuswar = PostVowelMarker 'ं'
wisarga = PostVowelMarker 'ः'

space = Space