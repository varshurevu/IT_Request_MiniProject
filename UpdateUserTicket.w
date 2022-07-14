&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
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
{ui/itrequest.i}.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iuserticket AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttuserTicket.
/* Local Variable Definitions ---                                       */

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 cmbDescription cmbcategory filStart ~
filEndDate cbStatus Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cmbDescription cmbcategory filStart ~
filEndDate cbStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "OK" 
    SIZE 15 BY 1.14.

DEFINE VARIABLE cbStatus       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Ticket Status" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Open","Pending","Hold ","Progress","Close" 
    DROP-DOWN-LIST
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cmbcategory    AS CHARACTER FORMAT "X(256)":U 
    LABEL "Category" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN-LIST
    SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cmbDescription AS CHARACTER FORMAT "X(256)":U 
    LABEL "Description" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN-LIST
    SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE filEndDate     AS CHARACTER FORMAT "X(256)":U 
    LABEL "EndDate" 
    VIEW-AS FILL-IN 
    SIZE 18 BY .95 NO-UNDO.

DEFINE VARIABLE filStart       AS DATE      FORMAT "99/99/99":U 
    LABEL "Start date" 
    VIEW-AS FILL-IN 
    SIZE 19 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-3
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 96 BY 6.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
    cmbDescription AT ROW 3.14 COL 64 COLON-ALIGNED WIDGET-ID 14
    cmbcategory AT ROW 3.38 COL 17 COLON-ALIGNED WIDGET-ID 12
    filStart AT ROW 5.52 COL 17 COLON-ALIGNED WIDGET-ID 16
    filEndDate AT ROW 5.76 COL 64 COLON-ALIGNED WIDGET-ID 4
    cbStatus AT ROW 7.19 COL 17 COLON-ALIGNED WIDGET-ID 10
    Btn_OK AT ROW 10.52 COL 61
    Btn_Cancel AT ROW 10.52 COL 84
    RECT-3 AT ROW 1.95 COL 3 WIDGET-ID 18
    SPACE(7.19) SKIP(3.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Update Ticket"
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
ASSIGN 
    FRAME gDialog:SCROLLABLE = FALSE
    FRAME gDialog:HIDDEN     = TRUE.

ASSIGN 
    filEndDate:READ-ONLY IN FRAME gDialog = TRUE.

ASSIGN 
    filStart:READ-ONLY IN FRAME gDialog = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Update Ticket */
    DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* OK */
    DO:
        DEFINE VARIABLE objusrTicket AS businesslogic.ItRequest.
        DEFINE VARIABLE objval       AS businesslogic.Validation.
        objval = NEW businesslogic.Validation().
        objusrTicket = NEW businesslogic.ItRequest().
        
        objusrTicket:iusrTicketId = iuserticket.
        if filEndDate:SCREEN-VALUE = "" AND cbStatus = "" THEN
        DO:
            MESSAGE "enter the correct input"
                VIEW-AS ALERT-BOX.
        END.
        ELSE
        DO:
            FIND FIRST ttuserTicket WHERE ttuserTicket.tusrTicketId = iuserticket NO-LOCK NO-ERROR.
        
            IF AVAILABLE ttuserTicket THEN 
            DO:
            
                ASSIGN
                    ttuserTicket.tticketStatus = cbStatus:SCREEN-VALUE
                    ttuserTicket.tenddate      = date(filEndDate:SCREEN-VALUE).
                           
            END.
            objusrTicket:UpdateUsrTicket(INPUT TABLE ttuserTicket). 
        END.  
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(objusrTicket)THEN DELETE OBJECT objusrTicket NO-ERROR.

        END FINALLY.
         
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
    DISPLAY cmbDescription cmbcategory filStart filEndDate cbStatus 
        WITH FRAME gDialog.
    ENABLE RECT-3 cmbDescription cmbcategory filStart filEndDate cbStatus Btn_OK 
        Btn_Cancel 
        WITH FRAME gDialog.
    VIEW FRAME gDialog.
    {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
    /*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.
    FIND FIRST ttuserTicket WHERE ttuserTicket.tusrTicketId = iuserticket NO-LOCK NO-ERROR.
    IF AVAILABLE ttuserTicket THEN
    DO:
        DO WITH FRAME {&frame-name} :
    
            ASSIGN
                filEndDate:SCREEN-value     = STRING(ttuserTicket.tenddate)
                cbStatus:SCREEN-VALUE       = STRING(ttuserTicket.tticketStatus)
                filStart:SCREEN-VALUE       = string(ttuserTicket.tstartdate).
               // cmbcategory:SCREEN-VALUE    = STRING (ttuserTicket.tcategory)
                //cmbDescription:SCREEN-VALUE = STRING (ttuserTicket.tTicketDiscription).
                
        END.
    END.

    /* Code placed here will execute AFTER standard behavior.    */
    CATCH e AS Progress.Lang.Error:
        MESSAGE e:GetMessage(1)
            VIEW-AS ALERT-BOX.
    END CATCH.

        

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

