{- Copyright 2017 NGLess Authors
 - License: MIT
 -}
module Citations
    ( collectCitations
    ) where

import qualified Data.Text as T
import Data.Maybe

import Modules
import Language

citations :: [(T.Text, T.Text)]
citations =
    [("assemble", "Li, D., Liu, C.M., Luo, R., Sadakane, K. and Lam, T.W., 2015. MEGAHIT: an ultra-fast single-node solution for large and complex metagenomics assembly via succinct de Bruijn graph. Bioinformatics, 31(10), pp.1674-1676.")
    ,("orf_find", "Hyatt, D., Chen, G.L., LoCascio, P.F., Land, M.L., Larimer, F.W. and Hauser, L.J., 2010. Prodigal: prokaryotic gene recognition and translation initiation site identification. BMC bioinformatics, 11(1), p.119.")
    ,("map", "Li, H., 2013. Aligning sequence reads, clone sequences and assembly contigs with BWA-MEM. arXiv preprint arXiv:1303.3997.")
    ]


collectCitations :: [Module] -> Script -> [T.Text]
collectCitations mods (Script _ sc) =
    let modCits = concatMap modCitations mods
        useCits = flip mapMaybe (snd <$> sc) $ \case
            Assignment _ (FunctionCall (FuncName f) _ _ _) -> lookup f citations
            _ -> Nothing
    in modCits ++ useCits
