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

DEFINE OUTPUT parameter inum AS INTEGER.
DEFINE OUTPUT PARAMETER TABLE FOR ttuser.
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
&Scoped-Define ENABLED-OBJECTS filName filLname filEmail filPhone Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS filName filLname filEmail filPhone 

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

DEFINE VARIABLE filEmail AS CHARACTER FORMAT "X(256)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.19 NO-UNDO.

DEFINE VARIABLE filLname AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.

DEFINE VARIABLE filName AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.19 NO-UNDO.

DEFINE VARIABLE filPhone AS CHARACTER FORMAT "X(256)":U 
     LABEL "Phone" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     filName AT ROW 2.91 COL 15 COLON-ALIGNED WIDGET-ID 2
     filLname AT ROW 2.91 COL 60 COLON-ALIGNED WIDGET-ID 4
     filEmail AT ROW 4.81 COL 15 COLON-ALIGNED WIDGET-ID 6
     filPhone AT ROW 4.81 COL 60 COLON-ALIGNED WIDGET-ID 8
     Btn_OK AT ROW 7.67 COL 42
     Btn_Cancel AT ROW 7.67 COL 66
     SPACE(2.19) SKIP(0.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add User"
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
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Add User */
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
        DEFINE VARIABLE objAdd      AS businesslogic.ItRequest.
        DEFINE VARIABLE objval      AS businesslogic.Validation.
        DEFINE VARIABLE lValidName  AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValidlName AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lresult     AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValiPhone  AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lvalidphone AS LOGICAL                  NO-UNDO.
        DEFINE VARIABLE lValidEmail AS LOGICAL                  NO-UNDO.
        
        objAdd = NEW businesslogic.ItRequest().
        objval = NEW businesslogic.Validation().
        
        if filName:SCREEN-VALUE = "" OR filLname:SCREEN-VALUE = "" OR filPhone:SCREEN-VALUE = "" OR filEmail:SCREEN-VALUE = "" THEN
        DO:
            MESSAGE "plx enter correct input"
                VIEW-AS ALERT-BOX.
            ASSIGN     
                filName:SCREEN-VALUE  = ""
                filLname:SCREEN-VALUE = ""
                filEmail:SCREEN-VALUE = ""
                filPhone:SCREEN-VALUE = "".
            
        END.
        else
        do:
            objAdd:usrNum = inum.
       
            objAdd:usrNum = NEXT-VALUE (nextUsrId).
            MESSAGE objAdd:usrNum
                VIEW-AS ALERT-BOX.
            FIND first ttUser WHERE ttUser.ttusrid = objAdd:usrNum NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttUser THEN
            DO:
                create ttUser.
                ASSIGN
                    ttUser.ttusrid     = objAdd:usrNum 
                    ttUser.ttfirstName = filName:SCREEN-VALUE
                    ttUser.ttlastName  = filLname:SCREEN-VALUE
                    ttUser.ttemaiid    = filEmail:SCREEN-VALUE
                    ttUser.ttphone     = filPhone:SCREEN-VALUE.
                
            END.
            objval:validEmail(INPUT filEmail:SCREEN-VALUE,output  lresult ).
            objval:valilname(input filLName:SCREEN-VALUE,OUTPUT lValidlName).
            objval:fnameValidation(INPUT filName:SCREEN-VALUE, output lValidName).
            objval:Phonevalidate(input filPhone:SCREEN-VALUE,OUTPUT lValiPhone).
            lValidEmail = objval:validEmail1(INPUT filEmail:SCREEN-VALUE).
            
            if  lValidName = TRUE  AND lValidlName = TRUE AND lValiPhone = TRUE AND lValidEmail = TRUE AND lresult = false  THEN
            DO:
                objAdd:AddUser(INPUT-OUTPUT TABLE ttUser). 
                
            END.
            
        END.
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
            ASSIGN
                filName:SCREEN-VALUE  = ""
                filLname:SCREEN-VALUE = ""
                filEmail:SCREEN-VALUE = ""
                filPhone:SCREEN-VALUE = "".
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(objAdd)THEN DELETE OBJECT objAdd NO-ERROR.

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
  DISPLAY filName filLname filEmail filPhone 
      WITH FRAME gDialog.
  ENABLE filName filLname filEmail filPhone Btn_OK Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

