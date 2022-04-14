module UCap.MapTests (testMap) where

import UCap.Domain
import UCap.Domain.PartMap
import UCap.Domain.StaticMap
import UCap.Coord
import UCap.Coord.PartMap
import UCap.Coord.StaticMap
import UCap.Op
import UCap.Op.PartMap

import Control.Monad.Writer
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

testMap = testGroup "MapC and MapE" [testQueue]

testQueue = testGroup "Queue"
  [testCase "MapC split" $
     split (uniC :: (SetC Int, SetC Int, SetC Int)) idC @?= Right uniC
  ,testCase "MapC split 2" $
     let c = mincap (idE, idE, insertE 5 ()) :: (SetC Int, SetC Int, SetC Int)
     in split idC c @?= Left c
  ,testCase "MapC split 3" $
     let c = mincap (idE, idE, insertE 5 ()) :: (SetC Int, SetC Int, SetC Int)
     in split uniC c @?= Right uniC
  ,testCase "MapC split 5" $
     split (uniC :: (SetC Int, SetC Int, SetC Int)) uniC @?= Right uniC
  ,testCase "MapC split 6" $
     split (idC :: (SetC Int, SetC Int, SetC Int)) uniC @?= Left uniC
  ,testCase "MapC split 7" $
     split (idC :: (SetC Int, SetC Int, SetC Int)) idC @?= Right idC
  ,testCase "Tuple <=?" $
     (idC :: (IntC, IntC, IntC)) <=? idC @?= True
  ,testCase "MapC <=? 1" $
     (idC :: (SetC Int, SetC Int, SetC Int)) <=? idC @?= True
  ,testCase "MapC <=? 2" $
     (uniC :: (SetC Int, SetC Int, SetC Int)) <=? idC @?= False
  ,testCase "MapC <=? 3" $
     (idC :: (SetC Int, SetC Int, SetC Int)) <=? uniC @?= True
  ,testCase "MapC <=? 4" $
     (uniC :: (SetC Int, SetC Int, SetC Int)) <=? uniC @?= True
  ,testCase "PartMap 1" $
     (resolveEffect "alpha" ((insPme ("alpha",1) "foo"))
                            (emptyPartMapG ["alpha","beta"] :: PartMapG String Int String (FreeC (FreeE String))))
     @?= Right (let PartMapG ins mod = emptyPartMapG ["alpha","beta"] :: PartMapG String Int String (FreeC (FreeE String))
                in PartMapG ins (initStaticMapG [(("alpha",1), mkTokenG "alpha")]))
  ,testCase "PartMap 2" $
     (mincap (insPme ("a",1) "foo") <=? (insPmc "a" :: PartMapC String Int (FreeC (FreeE String)))) @?= True
  ,testCase "PartMap 3" $
     ((idC :: PartMapC String Int (FreeC (FreeE String))) <=? mincap (insPme ("a",1) "foo")) @?= True
  ,testCase "PartMap 3.1" $
     split (uniC :: PartMapC String Int (FreeC (FreeE String))) (mincap (insPme ("a",1) "foo"))
     @?= Right uniC
  ,testCase "PartMap 4" $
     execWith (fullCaps :: Caps (PartMapC String Int (FreeC (FreeE String)))) (Map.empty) (pure (1,"foo") >>> insPmeOp "a")
     @?= Just (Identity (fullCaps, insPme ("a",1) "foo", ("a",1)))
  ]
