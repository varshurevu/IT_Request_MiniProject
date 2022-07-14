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
DEFINE INPUT PARAMETER usrid AS INTEGER.
DEFINE INPUT PARAMETER ticketid AS INTEGER.
DEFINE OUTPUT PARAMETER TABLE FOR ttuserTicket.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 cbTicketCat cbTicketDescrip filStart ~
FillEnd cbTicketStatus BtnSave Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS cbTicketCat cbTicketDescrip filStart ~
FillEnd cbTicketStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnSave AUTO-GO 
     LABEL "Save" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cbTicketCat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ticket Category" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "admin","hardware","infrastructure","firewall","server","network" 
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE cbTicketDescrip AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ticket Discription" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Desktop" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE cbTicketStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ticket Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "open","hold","pending","progress","close" 
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FillEnd AS DATE FORMAT "99/99/99":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.19 NO-UNDO.

DEFINE VARIABLE filStart AS DATE FORMAT "99/99/99":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     cbTicketCat AT ROW 3.86 COL 21 COLON-ALIGNED WIDGET-ID 4
     cbTicketDescrip AT ROW 3.86 COL 68 COLON-ALIGNED WIDGET-ID 6
     filStart AT ROW 5.76 COL 21 COLON-ALIGNED WIDGET-ID 8
     FillEnd AT ROW 5.76 COL 68 COLON-ALIGNED WIDGET-ID 10
     cbTicketStatus AT ROW 7.67 COL 21 COLON-ALIGNED WIDGET-ID 12
     BtnSave AT ROW 9.57 COL 58
     Btn_Cancel AT ROW 9.57 COL 82
     RECT-1 AT ROW 2.91 COL 4 WIDGET-ID 14
     SPACE(1.99) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add Ticket"
         DEFAULT-BUTTON BtnSave CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
ON WINDOW-CLOSE OF FRAME gDialog /* Add Ticket */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSave gDialog
ON CHOOSE OF BtnSave IN FRAME gDialog /* Save */
DO:
        DEFINE VARIABLE objAddTicket AS businesslogic.ItRequest.
        objAddTicket = NEW businesslogic.ItRequest().
        
        //DEFINE VARIABLE iticket    AS INTEGER                 NO-UNDO.
        objAddTicket:iusrTicketId = ticketid.
//        iticket = NEXT-VALUE (nextUsrId).
        
        if cbTicketStatus:SCREEN-VALUE = ? THEN
        DO:
            MESSAGE "plx enter correct input"
                VIEW-AS ALERT-BOX.
                
        END.
        else
        do:
            
            FIND first ttuserTicket WHERE ttuserTicket.tusrTicketId = ticketid NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttuserTicket THEN
            DO:
                create ttuserTicket.
            
            
                ASSIGN
                    ttuserTicket.tusrid             = usrid
                    ttuserTicket.tusrTicketId       = ticketid
                  //  ttuserTicket.tticketId     = iticket
                    ttuserTicket.tTicketDiscription = cbTicketDescrip:SCREEN-VALUE
                    ttuserTicket.tcategory          = cbTicketCat:SCREEN-VALUE
                    ttuserTicket.tticketStatus      = cbTicketStatus:SCREEN-VALUE.
            END.
            objAddTicket:AddTicketusr(INPUT TABLE ttuserTicket). 
        END.
        
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(objAddTicket)THEN DELETE OBJECT objAddTicket NO-ERROR.

        END FINALLY.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTicketCat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTicketCat gDialog
ON VALUE-CHANGED OF cbTicketCat IN FRAME gDialog /* Ticket Category */
DO:
        DEFINE VARIABLE objcat AS businesslogic.Validation.
        objcat = NEW businesslogic.Validation().
        objcat:cmbcategory(INPUT cbTicketCat:SCREEN-VALUE, INPUT cbTicketDescrip:HANDLE,INPUT cbTicketStatus:HANDLE).
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
  DISPLAY cbTicketCat cbTicketDescrip filStart FillEnd cbTicketStatus 
      WITH FRAME gDialog.
  ENABLE RECT-1 cbTicketCat cbTicketDescrip filStart FillEnd cbTicketStatus 
         BtnSave Btn_Cancel 
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
    run puserTicket.

    /* Code placed here will execute AFTER standard behavior.    */


    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE puserTicket gDialog 
PROCEDURE puserTicket :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    /*    for each ticket:                                                        */
    /*        cbTicketDescrip:ADD-Last(Ticket.ticketdescription) IN FRAME gDialog.*/
    /*    END.                                                                    */
    /*    for each Ticket:                                            */
    /*        cbTicketCat:ADD-LAST (Ticket.category) in frame gDialog.*/
    /*    END.                                                        */
    /*                                                                            */
    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

