// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ true, true, false, false, true, false, false, false, false, false, false, false, false, true, false, false, false, true, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, true, true, true, false, false, false, false, true, true, true, false, false, false, false, false, true, false, false, false, true, false, false, false, false, true, false, false, false, true, false, false, true, false, true, true, false, false, true, true, true, false, false, false, false, true, false, true, false, false, false, false, false, true, false, false, false ];
var arrayMetadata    = [ [ "1", "6303248007_A", "6303248007_A" ], [ "2", "6303248077_F", "6303248077_F" ], [ "3", "7656774098_C", "7656774098_C" ], [ "4", "7656774098_G", "7656774098_G" ], [ "5", "7656774098_H", "7656774098_H" ], [ "6", "8803836064_A", "8803836064_A" ], [ "7", "8803836064_B", "8803836064_B" ], [ "8", "8803836064_J", "8803836064_J" ], [ "9", "6303248007_B", "6303248007_B" ], [ "10", "6303248007_E", "6303248007_E" ], [ "11", "6303248007_G", "6303248007_G" ], [ "12", "6303248077_E", "6303248077_E" ], [ "13", "6303248077_I", "6303248077_I" ], [ "14", "7668775006_C", "7668775006_C" ], [ "15", "7668775006_D", "7668775006_D" ], [ "16", "7668775006_G", "7668775006_G" ], [ "17", "7668775006_H", "7668775006_H" ], [ "18", "7668775006_L", "7668775006_L" ], [ "19", "8803836063_A", "8803836063_A" ], [ "20", "6303248007_C", "6303248007_C" ], [ "21", "6303248077_H", "6303248077_H" ], [ "22", "7656774098_A", "7656774098_A" ], [ "23", "7656774098_E", "7656774098_E" ], [ "24", "7656774098_F", "7656774098_F" ], [ "25", "8803836064_C", "8803836064_C" ], [ "26", "8803836064_D", "8803836064_D" ], [ "27", "8803836064_G", "8803836064_G" ], [ "28", "8803836064_L", "8803836064_L" ], [ "29", "6303248007_D", "6303248007_D" ], [ "30", "6303248007_H", "6303248007_H" ], [ "31", "6303248077_G", "6303248077_G" ], [ "32", "6303248077_K", "6303248077_K" ], [ "33", "7668775006_A", "7668775006_A" ], [ "34", "7668775006_B", "7668775006_B" ], [ "35", "7668775006_E", "7668775006_E" ], [ "36", "7668775006_F", "7668775006_F" ], [ "37", "7668775006_J", "7668775006_J" ], [ "38", "8803836063_C", "8803836063_C" ], [ "39", "6303248007_F", "6303248007_F" ], [ "40", "6303248007_J", "6303248007_J" ], [ "41", "6303248077_A", "6303248077_A" ], [ "42", "7668775006_K", "7668775006_K" ], [ "43", "8803836063_E", "8803836063_E" ], [ "44", "8803836063_F", "8803836063_F" ], [ "45", "8803836063_I", "8803836063_I" ], [ "46", "8803836063_J", "8803836063_J" ], [ "47", "6303248007_I", "6303248007_I" ], [ "48", "6303248077_D", "6303248077_D" ], [ "49", "6303248077_J", "6303248077_J" ], [ "50", "7656774098_D", "7656774098_D" ], [ "51", "7656774098_K", "7656774098_K" ], [ "52", "7656774098_L", "7656774098_L" ], [ "53", "8803836063_B", "8803836063_B" ], [ "54", "6303248007_K", "6303248007_K" ], [ "55", "6303248077_B", "6303248077_B" ], [ "56", "6303248077_L", "6303248077_L" ], [ "57", "7656774098_B", "7656774098_B" ], [ "58", "7656774098_I", "7656774098_I" ], [ "59", "7656774098_J", "7656774098_J" ], [ "60", "8803836063_D", "8803836063_D" ], [ "61", "6303248007_L", "6303248007_L" ], [ "62", "6303248077_C", "6303248077_C" ], [ "63", "7668775006_I", "7668775006_I" ], [ "64", "8803836063_G", "8803836063_G" ], [ "65", "8803836063_H", "8803836063_H" ], [ "66", "8803836063_K", "8803836063_K" ], [ "67", "8803836063_L", "8803836063_L" ], [ "68", "8803836064_K", "8803836064_K" ], [ "69", "8938116002_A", "8938116002_A" ], [ "70", "8938116002_G", "8938116002_G" ], [ "71", "8938116002_I", "8938116002_I" ], [ "72", "8938116017_G", "8938116017_G" ], [ "73", "8938116017_K", "8938116017_K" ], [ "74", "8938116002_B", "8938116002_B" ], [ "75", "8938116002_H", "8938116002_H" ], [ "76", "8938116002_J", "8938116002_J" ], [ "77", "8938116017_F", "8938116017_F" ], [ "78", "8938116017_H", "8938116017_H" ], [ "79", "8938116017_L", "8938116017_L" ], [ "80", "8938116002_C", "8938116002_C" ], [ "81", "8938116002_E", "8938116002_E" ], [ "82", "8938116002_K", "8938116002_K" ], [ "83", "8938116017_A", "8938116017_A" ], [ "84", "8938116017_C", "8938116017_C" ], [ "85", "8938116017_I", "8938116017_I" ], [ "86", "8938116002_D", "8938116002_D" ], [ "87", "8938116002_F", "8938116002_F" ], [ "88", "8938116002_L", "8938116002_L" ], [ "89", "8938116017_B", "8938116017_B" ], [ "90", "8938116017_D", "8938116017_D" ], [ "91", "8938116017_J", "8938116017_J" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
	var success = false;
	i = 0; 
	/* Some of this looping could already be cached in reportInit() */
	while( (!success) & (i < ssrules.length) ) {
	    selector = ssrules[i].selectorText;  // The selector 
            if (!selector) 
		continue; // Skip @import and other nonstyle rules
            if (selector == (".aqm" + reportObjId)) {
		success = true; 
		ssrules[i].style.cssText = cssText[0+status];
	    } else {
		i++;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
