&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT parameter inum AS INTEGER.
/* Local Variable Definitions ---                                       */
{ui/itrequest.i}.

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog
&Scoped-define BROWSE-NAME brwUser

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUser

/* Definitions for BROWSE brwUser                                       */
&Scoped-define FIELDS-IN-QUERY-brwUser ttUser.ttusrid ttUser.ttfirstName ttUser.ttlastName ttUser.ttphone ttUser.ttemaiid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwUser   
&Scoped-define SELF-NAME brwUser
&Scoped-define QUERY-STRING-brwUser FOR EACH ttUser
&Scoped-define OPEN-QUERY-brwUser OPEN QUERY {&SELF-NAME} FOR EACH ttUser.
&Scoped-define TABLES-IN-QUERY-brwUser ttUser
&Scoped-define FIRST-TABLE-IN-QUERY-brwUser ttUser


/* Definitions for DIALOG-BOX gDialog                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-gDialog ~
    ~{&OPEN-QUERY-brwUser}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS filName filLast btnFilter filPhone filEmail ~
brwUser Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS filName filLast filPhone filEmail 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFilter 
    LABEL "Filter" 
    SIZE 16 BY 1.43.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE filEmail AS CHARACTER FORMAT "X(256)":U 
    LABEL "Email" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.19 NO-UNDO.

DEFINE VARIABLE filLast  AS CHARACTER FORMAT "X(256)":U 
    LABEL "Last Name" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.19 NO-UNDO.

DEFINE VARIABLE filName  AS CHARACTER FORMAT "X(256)":U 
    LABEL "First Name" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.19
    FONT 6 NO-UNDO.

DEFINE VARIABLE filPhone AS CHARACTER FORMAT "X(256)":U 
    LABEL "Phone" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwUser FOR 
    ttUser SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwUser gDialog _FREEFORM
    QUERY brwUser DISPLAY
    ttUser.ttusrid Label "userId"
    ttUser.ttfirstName LABEL "first name"
    ttUser.ttlastName  LABEL "last name"
    ttUser.ttphone LABEL "phone"
    ttUser.ttemaiid LABEL "email"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 75 BY 5.48 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
    filName AT ROW 2.91 COL 11 COLON-ALIGNED WIDGET-ID 4
    filLast AT ROW 2.91 COL 52 COLON-ALIGNED WIDGET-ID 6
    btnFilter AT ROW 2.91 COL 93 WIDGET-ID 12
    filPhone AT ROW 4.57 COL 11 COLON-ALIGNED WIDGET-ID 8
    filEmail AT ROW 4.81 COL 52 COLON-ALIGNED WIDGET-ID 10
    brwUser AT ROW 6.24 COL 4 WIDGET-ID 200
    Btn_OK AT ROW 10.52 COL 81
    Btn_Cancel AT ROW 10.52 COL 100
    "ADVANCE SEARCH" VIEW-AS TEXT
    SIZE 24 BY 1.19 AT ROW 1.24 COL 37 WIDGET-ID 200
    FGCOLOR 0 FONT 6
    SPACE(55.59) SKIP(9.85)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    BGCOLOR 14 
    TITLE "Advance Search"
    DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
/* BROWSE-TAB brwUser filEmail gDialog */
ASSIGN 
    FRAME gDialog:SCROLLABLE = FALSE
    FRAME gDialog:HIDDEN     = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwUser
/* Query rebuild information for BROWSE brwUser
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUser.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwUser */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Advance Search */
    DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFilter gDialog
ON CHOOSE OF btnFilter IN FRAME gDialog /* Filter */
    DO:
        DEFINE VARIABLE ofilter AS businesslogic.ItRequest.
        DEFINE VARIABLE cfirstname AS CHARACTER               NO-UNDO.
        DEFINE VARIABLE clastname  AS CHARACTER               NO-UNDO.
        DEFINE VARIABLE cPhone     AS CHARACTER               NO-UNDO.
        DEFINE VARIABLE cemail     AS CHARACTER               NO-UNDO.
        ofilter = new businesslogic.ItRequest().
        
        if filName:SCREEN-VALUE = "" AND filLast:SCREEN-VALUE = "" AND filPhone:SCREEN-VALUE = "" AND filEmail:SCREEN-VALUE = "" THEN
        DO:
            MESSAGE "enter correct input"
            VIEW-AS ALERT-BOX.
            ASSIGN
                filName:SCREEN-VALUE  = ""
                filLast:SCREEN-VALUE  = ""
                filEmail:SCREEN-VALUE = ""
                filPhone:SCREEN-VALUE = "".
            EMPTY TEMP-TABLE ttUser.
            OPEN QUERY brwUser FOR EACH ttUser  NO-LOCK.
        END.
        ELSE
        DO:
/*            ofilter:cFirstName = filName:SCREEN-VALUE.*/
/*            ofilter:clastName = filLast:SCREEN-VALUE. */
/*            ofilter:cPhone =filPhone:SCREEN-VALUE.    */
/*            ofilter:cEmail = filEmail:SCREEN-VALUE.   */
/*            ofilter:Advancefilter(OUTPUT TABLE ttUser).*/ //static

             cfirstname = filName:SCREEN-VALUE.
            clastname =  filLast:SCREEN-VALUE.
            cPhone  =filPhone:SCREEN-VALUE.
            cemail = filEmail:SCREEN-VALUE.
            ofilter:getUserDetails(INPUT cfirstname, INPUT clastname,INPUT  cPhone, INPUT cemail, OUTPUT TABLE ttUser).
            OPEN QUERY brwUser FOR each ttUser.
        END.
        CATCH e AS Progress.Lang.Error:
            
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
            ASSIGN     
                filName:SCREEN-VALUE  = ""
                filLast:SCREEN-VALUE  = ""
                filEmail:SCREEN-VALUE = ""
                filPhone:SCREEN-VALUE = "".
            EMPTY TEMP-TABLE ttUser.
            OPEN QUERY brwUser FOR EACH ttUser NO-LOCK. 
        END CATCH.
        
        FINALLY:
            IF VALID-OBJECT(ofilter)THEN DELETE OBJECT ofilter NO-ERROR.

        END FINALLY.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
    DO:
        inum = ttUser.ttusrid.
        
        CATCH e AS Progress.Lang.Error:
            MESSAGE "search something"
            VIEW-AS ALERT-BOX.
/*            MESSAGE e:GetMessage(1)*/
/*                VIEW-AS ALERT-BOX. */
        END CATCH.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwUser
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Hide all frames. */
    HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     ENABLE the User Interface
      Parameters:  <none>
      Notes:       Here we display/view/enable the widgets in the
                   user-interface.  In addition, OPEN all queries
                   associated with each FRAME and BROWSE.
                   These statements here are based on the "Other 
                   Settings" section of the widget Property Sheets.
    ------------------------------------------------------------------------------*/
    DISPLAY filName filLast filPhone filEmail 
        WITH FRAME gDialog.
    ENABLE filName filLast btnFilter filPhone filEmail brwUser Btn_OK Btn_Cancel 
        WITH FRAME gDialog.
    VIEW FRAME gDialog.
    {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

