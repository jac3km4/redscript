
/* multiply all crafting XP by 100 */
@insert(CraftingSystem)
private final void ProcessCraftSkill(Int32 xpAmount, StatsObjectID craftedItem) {
  ref<ExperiencePointsEvent> xpEvent = new ExperiencePointsEvent();
  xpEvent.amount = xpAmount * 100;
  xpEvent.type = gamedataProficiencyType.Crafting;
  GetPlayer(this.GetGameInstance()).QueueEvent(xpEvent);
}

/* disable fake underwear when removing clothes */
@insert(EquipmentSystemPlayerData)
private final Bool EvaluateUnderwearVisibility(ItemID unequippedItem) {
  return false;
}

/* allow disassembling of all items */
@insert(CraftingSystem)
public final const Bool CanItemBeDisassembled(wref<gameItemData> itemData) {
  return true;
}

/* allow unequipping of all items */
@insert(RPGManager)
public final static Bool CanPartBeUnequipped(ItemID itemID) {
  return true;
}

/* add main menu options */
@insert(SingleplayerMenuGameController)
private void PopulateMenuItemList() {
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
private final array<wref<gameItemData>> GetSellableJunk() {
    array<wref<gameItemData>> result;
    array<ref<gameItemData>> sellableItems = this.m_VendorDataManager.GetItemsPlayerCanSell();
    Int32 i = 0;
    while(i < ArraySize(sellableItems)) {
        gamedataItemType type = RPGManager.GetItemRecord(sellableItems[i].GetID()).ItemType().Type();
        if(Equals(type, gamedataItemType.Gen_Junk) || Equals(type, gamedataItemType.Con_Edible)) {
            wref<gameItemData> tmp = sellableItems[i];
            ArrayPush(result, tmp);
        };
        i += 1;
    };
    return result;
}
