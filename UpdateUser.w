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

/* Parameters Definitions ---                                           */
{ui/itrequest.i}.

DEFINE INPUT parameter inum AS INTEGER.
DEFINE INPUT PARAMETER TABLE FOR ttUser.
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
&Scoped-Define ENABLED-OBJECTS FillName fillLname fillEmail fillPhone ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FillName fillLname fillEmail fillPhone 

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

DEFINE VARIABLE fillEmail AS CHARACTER FORMAT "X(256)":U 
    LABEL "Email" 
    VIEW-AS FILL-IN 
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE fillLname AS CHARACTER FORMAT "X(256)":U 
    LABEL "Last Name" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.19 NO-UNDO.

DEFINE VARIABLE FillName  AS CHARACTER FORMAT "X(256)":U 
    LABEL "First Name" 
    VIEW-AS FILL-IN 
    SIZE 21 BY .95 NO-UNDO.

DEFINE VARIABLE fillPhone AS CHARACTER FORMAT "X(256)":U 
    LABEL "Phone" 
    VIEW-AS FILL-IN 
    SIZE 20 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
    FillName AT ROW 1.95 COL 10.2 COLON-ALIGNED WIDGET-ID 2
    fillLname AT ROW 1.95 COL 49 COLON-ALIGNED WIDGET-ID 4
    fillEmail AT ROW 3.86 COL 10 COLON-ALIGNED WIDGET-ID 6
    fillPhone AT ROW 3.86 COL 49 COLON-ALIGNED WIDGET-ID 8
    Btn_OK AT ROW 10.52 COL 82
    Btn_Cancel AT ROW 10.52 COL 102
    SPACE(4.99) SKIP(0.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "<insert SmartDialog title>"
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
ON WINDOW-CLOSE OF FRAME gDialog /* <insert SmartDialog title> */
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
        DEFINE VARIABLE lValidName   AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValidlName  AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lresult      AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValiPhone   AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lvalidphone  AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValidEmail  AS LOGICAL                  NO-UNDO.
        objval = new businesslogic.Validation().
   
        objusrTicket = NEW businesslogic.ItRequest().
        
        objusrTicket:usrNum = inum.
        FIND FIRST ttUser WHERE ttUser.ttusrid = objusrTicket:usrNum NO-LOCK NO-ERROR.
        
        IF AVAILABLE ttUser THEN 
        DO:
            ASSIGN
                ttUser.ttusrid     = objusrTicket:usrNum 
                ttUser.ttfirstName = FillName:SCREEN-VALUE
                ttUser.ttlastName  = fillLname:SCREEN-VALUE
                ttUser.ttphone     = fillPhone:SCREEN-VALUE
                ttUser.ttemaiid    = fillEmail:SCREEN-VALUE.
                
/*            MESSAGE ttUser.ttusrid ".w end"*/
/*                VIEW-AS ALERT-BOX.         */
                
                       
        END.
        objval:valilname(input fillLName:SCREEN-VALUE,OUTPUT lValidlName).
        objval:fnameValidation(INPUT FillName:SCREEN-VALUE, output lValidName).
        objval:Phonevalidate(input fillPhone:SCREEN-VALUE,OUTPUT lValiPhone).
        lValidEmail = objval:validEmail1(INPUT fillEmail:SCREEN-VALUE).
            
         
            if  lValidName = TRUE  AND lValidlName = TRUE AND lValiPhone = TRUE AND lValidEmail = TRUE THEN
            DO:
                objusrTicket:updateUser(INPUT TABLE ttUser).  
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
    DISPLAY FillName fillLname fillEmail fillPhone 
        WITH FRAME gDialog.
    ENABLE FillName fillLname fillEmail fillPhone Btn_OK Btn_Cancel 
        WITH FRAME gDialog.
    VIEW FRAME gDialog.
    {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog
PROCEDURE initializeObject:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.
    FIND FIRST ttUser WHERE ttUser.ttusrid = inum NO-LOCK NO-ERROR.
    IF AVAILABLE ttUser THEN
    DO:
        DO WITH FRAME {&frame-name} :
    
            ASSIGN
                FillName:SCREEN-value  = STRING(ttUser.ttfirstName)
                fillLname:SCREEN-VALUE = STRING(ttUser.ttlastName)
                fillEmail:SCREEN-VALUE = STRING(ttUser.ttemaiid)
                fillPhone:SCREEN-VALUE = STRING (ttUser.ttphone).
                
        END.
    END.
/* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


