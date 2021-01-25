library(dashboardthemes)

header_color <- "#4d4d4e"#"#696562"#"#000000"
body_color <- "#5e5e5e"#"#f2a900" #"#454342"# "#191919"
font_color <- "#f2a900" # bitcoin
sidebar_color <- header_color
tab_box_color <- "#f0f0f0"# "#222222"
box_color <- tab_box_color

customTheme <- shinyDashboardThemeDIY(
  appFontFamily = "Arial",
  appFontColor = "",
  logoBackColor = header_color,
  bodyBackColor = body_color,
  headerButtonBackColor = header_color,
  headerButtonIconColor = font_color,
  headerButtonBackColorHover = header_color,
  headerButtonIconColorHover = font_color,
  headerBackColor = header_color,
  headerBoxShadowColor = header_color,
  headerBoxShadowSize = "0px 0px 0px",
  sidebarBackColor = sidebar_color,
  sidebarPadding = 0,
  sidebarShadowRadius = "0px 0px 0px",
  sidebarShadowColor = sidebar_color,
  sidebarMenuBackColor = sidebar_color,
  sidebarMenuPadding = sidebar_color,
  sidebarMenuBorderRadius = 0,
  sidebarUserTextColor = "#ff0000",
  sidebarSearchBackColor = "white",#"#ff0000",
  sidebarSearchIconColor = "black",
  sidebarSearchBorderColor = "white",
  sidebarTabTextColor = font_color,
  sidebarTabTextSize = 13,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "blue",
  sidebarTabBorderWidth = 0,
  sidebarTabBackColorSelected = sidebar_color, # czy podswietlac wybrane
  sidebarTabTextColorSelected = font_color,
  sidebarTabRadiusSelected = "0px 0px 0px 0px",
  sidebarTabTextColorHover = font_color,
  sidebarTabBackColorHover = sidebar_color,
  sidebarTabBorderStyleHover = "none none none none",
  sidebarTabBorderColorHover = body_color,
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 0px 0px 0px",
  boxBackColor = box_color,
  boxBorderRadius = 0,
  boxShadowSize = "0px 0px 0px",
  boxShadowColor = box_color,
  boxTitleSize = 16,
  boxDefaultColor = box_color,
  boxPrimaryColor = box_color,
  boxSuccessColor = font_color,
  boxWarningColor = box_color,
  boxDangerColor = box_color,
  tabBoxTabColor = tab_box_color,
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = font_color,
  tabBoxTabTextColorSelected = font_color,
  tabBoxBackColor = tab_box_color,
  tabBoxHighlightColor = tab_box_color,
  tabBoxBorderRadius = 0,
  buttonBackColor = tab_box_color,
  buttonTextColor = font_color,
  buttonBorderColor = tab_box_color,
  buttonBorderRadius = 0,
  buttonBackColorHover = tab_box_color,
  buttonTextColorHover = font_color,
  buttonBorderColorHover = tab_box_color,
  buttonHeight = 34,
  buttonPadding = "6px 12px",
  textboxBackColor = "#ffffff", # kolor okienka np. z Open
  textboxBorderColor = tab_box_color,
  textboxBorderRadius = 2,
  textboxBackColorSelect = "",
  textboxBorderColorSelect = "",
  textboxHeight = 34,
  textboxPadding = "6px 12px",
  tableBackColor = tab_box_color,
  tableBorderColor = "#d8d8d8",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1,
  primaryFontColor = "auto",
  successFontColor = "auto",
  warningFontColor = "auto",
  dangerFontColor = "auto",
  infoFontColor = "auto",
  boxInfoColor = "auto"
)

save(list = c("customTheme"), file="C:/Users/rogow/OneDrive/Dokumenty/UWr/Programowanie i analiza danych w R/BTC/theme.RData")