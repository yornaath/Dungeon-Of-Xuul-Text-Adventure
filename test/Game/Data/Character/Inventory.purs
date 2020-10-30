module Test.Game.Data.Character.Inventory where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Map as M
import Data.Tuple (Tuple(..))
import Game.Data.Character.Inventory (GearSlot(..), Inventory, InventoryItem(..), equip)
import Game.Data.Experience (Level(..))
import Game.Data.Item.Equipment (Equipment(..))
import Game.Data.Stats (emptyStats, mkStats)
import Test.Unit (TestF, test, suite)
import Test.Unit.Assert as Assert



inventoryTests :: Free TestF Unit
inventoryTests = do
  suite "Game.Data.Character.Inventory" do

    suite "InventoryItem" do
      test "eq" do 
        let ha = Helmet { name: "foo", description: "foo", stats: emptyStats, levelRequirement: Level 1 }
        let hb = Helmet { name: "foo", description: "foo", stats: emptyStats, levelRequirement: Level 1 }
        let hc = Helmet { name: "foos", description: "foo", stats: emptyStats, levelRequirement: Level 1 }
        let hd = Helmet { name: "foo", description: "foo", stats: mkStats 0 0 0 0 1, levelRequirement: Level 1 }
        Assert.equal (ha == hb) true
        Assert.equal (not $ ha == hc) true 
        Assert.equal (not $ ha == hd) true 

    suite "equip" do

      test "Should return item in slot to inventory" do
        let inventory = {carrying : [InventoryEquipment helmetB], equiped: M.fromFoldable [Tuple Head helmetA]} :: Inventory
        let expected = {carrying : [InventoryEquipment helmetA], equiped: M.fromFoldable [Tuple Head helmetB]} :: Inventory
        case equip Head helmetB inventory of 
          Left error -> Assert.assert error false
          Right inventoryAfterEquip -> do
            Assert.equal expected inventoryAfterEquip
      
      test "Equiping a two handed weapon should unslot offhand and return to inventory" do
        let inventory = {carrying : [InventoryEquipment greatSwordA], equiped: M.fromFoldable [Tuple OffHand daggerA]} :: Inventory
        let expected = {carrying : [InventoryEquipment daggerA], equiped: M.fromFoldable [Tuple MainHand greatSwordA]} :: Inventory
        case equip MainHand greatSwordA inventory of 
          Left error -> Assert.assert error false
          Right inventoryAfterEquip -> do
            Assert.equal expected inventoryAfterEquip


helmetA :: Equipment
helmetA = Helmet { name: "ha", description: "helmet a", stats: emptyStats, levelRequirement: Level 1 }

helmetB :: Equipment
helmetB = Helmet { name: "hb", description: "helmet b", stats: emptyStats, levelRequirement: Level 1 }

daggerA :: Equipment
daggerA = Dagger { name: "dirge a", description: "dagger a", stats: emptyStats, damage: 99, levelRequirement: Level 1 }

daggerB :: Equipment
daggerB = Dagger { name: "dirge b", description: "dagger B", stats: emptyStats, damage: 99, levelRequirement: Level 1 }

greatSwordA :: Equipment
greatSwordA = GreatSword { name: "greatsword a", description: "greatsword a", stats: emptyStats, damage: 150, levelRequirement: Level 1 }
