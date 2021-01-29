
/* multiply all crafting XP by 100 */
@insert(CraftingSystem)
private final ProcessCraftSkill(xpAmount: Int32, craftedItem: StatsObjectID) {
  let xpEvent = new ExperiencePointsEvent();
  xpEvent.amount = xpAmount * 100;
  xpEvent.type = gamedataProficiencyType.Crafting;
  GetPlayer(this.GetGameInstance()).QueueEvent(xpEvent);
}

/* disable fake underwear when removing clothes */
@insert(EquipmentSystemPlayerData)
private final EvaluateUnderwearVisibility(unequippedItem: ItemID): Bool {
  return false;
}

/* allow disassembling of all items */
@insert(CraftingSystem)
public final const CanItemBeDisassembled(itemData: wref<gameItemData>): Bool {
  return true;
}

/* allow unequipping of all items */
@insert(RPGManager)
public final static CanPartBeUnequipped(itemID: ItemID): Bool {
  return true;
}

/* add main menu options */
@insert(SingleplayerMenuGameController)
private PopulateMenuItemList() {
  if(this.m_savesCount > 0) {
    this.AddMenuItem(GetLocalizedText("UI-ScriptExports-Continue0"), PauseMenuAction.QuickLoad);
  };
  this.AddMenuItem(GetLocalizedText("UI-ScriptExports-NewGame0"), n"OnNewGame");
  this.AddMenuItem(GetLocalizedText("UI-ScriptExports-LoadGame0"), n"OnLoadGame");
  this.AddMenuItem(GetLocalizedText("UI-Labels-Settings"), n"OnSwitchToSettings");
  this.AddMenuItem(GetLocalizedText("UI-Labels-Credits"), n"OnSwitchToCredits");
  this.AddMenuItem("DEBUG NEW GAME", n"OnDebug");
  this.AddMenuItem("TOGGLE GOG MENU", n"OnGOGProfile");
  this.m_menuListController.Refresh();
  this.SetCursorOverWidget(inkCompoundRef.GetWidgetByIndex(this.m_menuList, 0));
}

/* sell edibles as junk automatically at vendors */
@insert(FullscreenVendorGameController)
private final GetSellableJunk(): array<wref<gameItemData>> {
    let result: array<wref<gameItemData>>;
    let sellableItems = this.m_VendorDataManager.GetItemsPlayerCanSell();
    let i = 0;
    while(i < ArraySize(sellableItems)) {
        let type = RPGManager.GetItemRecord(sellableItems[i].GetID()).ItemType().Type();
        if(Equals(type, gamedataItemType.Gen_Junk) || Equals(type, gamedataItemType.Con_Edible)) {
            let tmp: wref<gameItemData> = sellableItems[i];
            ArrayPush(result, tmp);
        };
        i += 1;
    };
    return result;
}
