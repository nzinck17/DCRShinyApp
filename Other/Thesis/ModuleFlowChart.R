require(DiagrammeR)

grViz("

digraph circo {

graph [layout = circo]

# add node statements
node[shape = box, style = filled, fillcolor = red]
App

node[shape = box, fillcolor = green]
Home; Time; TimeDepth; Regress; RegressDepth; ProfileHeatmap; ProfileLine; ProfileSummary; 
MapPlot; Export; ReportAWQ; ReportMWQ; ReportCustom;

node[shape = box, fillcolor = orange]

# add edge statements

  App -> Home; 
  App -> Time; 
  App -> TimeDepth; 
  App -> Regress; 
  App -> RegressDepth; 
  App -> ProfileHeatmap; 
  App -> ProfileLine; 
  App -> ProfileSummary; 
  App -> MapPlot; 
  App -> Export; 
  App -> ReportAWQ; 
  App -> ReportMWQ; 
  App -> ReportCustom;

    Time-> SiteMap1;
    Time-> PlotTime1;
    Time-> Summary1;
    Time-> SiteSelection1 -> CheckboxSelectAll1;
    Time-> CheckboxSelectAll1;

    TimeDepth-> SiteMap2;
    TimeDepth-> PlotTime2;
    TimeDepth-> Summary2;
    TimeDepth-> SiteSelection2 -> CheckboxSelectAll2;
    TimeDepth-> CheckboxSelectAll2;

    Regress-> SiteMap3;
    Regress-> PlotTime3;
    Regress-> Summary3;
    Regress-> SiteSelection3 -> CheckboxSelectAll3;
    Regress-> CheckboxSelectAll3;

    RegressDepth-> SiteMap4;
    RegressDepth-> PlotTime4;
    RegressDepth-> Summary4;
    RegressDepth-> SiteSelection4 -> CheckboxSelectAll4;
    RegressDepth-> CheckboxSelectAll4;

    ProfileHeatmap -> PlotProfHeatCustom
    
    ProfileLine -> PlotProfLineCustom

    Export-> SiteSelection5 -> CheckboxSelectAll5;
    Export-> CheckboxSelectAll5;


}
")