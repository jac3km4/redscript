
@insert(CraftingMainGameController)
private final void UpgradeItem() {
  ref<UpgradeItemRequest> upgradeItemRequest = new UpgradeItemRequest();
  upgradeItemRequest.owner = this.m_player;
  upgradeItemRequest.itemID = InventoryItemData.GetID(this.m_selectedItemData);
  Int32 i = 0;
  while (i < 10) {
    this.m_craftingSystem.QueueRequest(upgradeItemRequest);
    i += 1;
  };
  this.RefreshUI();
  this.PlaySound(n"Item", n"OnCraftted");
}

@insert(CraftingSystem)
private final void ProcessCraftSkill(Int32 xpAmount, StatsObjectID craftedItem) {
  Int32 i;
  TweakDBID rewardID;
  ref<ExperiencePointsEvent> xpEvent;
  xpEvent = new ExperiencePointsEvent();
  xpEvent.amount = xpAmount * 100;
  xpEvent.type = gamedataProficiencyType.Crafting;
  GetPlayer(this.GetGameInstance()).QueueEvent(xpEvent);
}

@insert(EquipmentSystemPlayerData)
private final Bool EvaluateUnderwearVisibility(ItemID unequippedItem) {
  return false;
}

@insert(CraftingSystem)
public final const Bool CanItemBeDisassembled(wref<gameItemData> itemData) {
  return true;
}

@insert(RPGManager)
public final static Bool CanPartBeUnequipped(ItemID itemID) {
  return true;
}

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

@insert(FullscreenVendorGameController)
private final array<wref<gameItemData>> GetSellableJunk() {
    Int32 i;
    array<wref<gameItemData>> result;
    array<ref<gameItemData>> sellableItems;
    sellableItems = this.m_VendorDataManager.GetItemsPlayerCanSell();
    i = 0;
    while(i < ArraySize(sellableItems)) {
        if(Equals(RPGManager.GetItemRecord(sellableItems[i].GetID()).ItemType().Type(), gamedataItemType.Gen_Junk)
            || Equals(RPGManager.GetItemRecord(sellableItems[i].GetID()).ItemType().Type(), gamedataItemType.Con_Edible)) {
            wref<gameItemData> tmp = sellableItems[i];
            ArrayPush(result, tmp);
        };
        i += 1;
    };
    return result;
}
