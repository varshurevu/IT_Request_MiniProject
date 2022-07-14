&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

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

/* Local Variable Definitions ---                                       */
{ui/itrequest.i}.
DEFINE QUERY quserTicket FOR  ttuserTicket SCROLLING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwuserTicket

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttuserTicket

/* Definitions for BROWSE brwuserTicket                                 */
&Scoped-define FIELDS-IN-QUERY-brwuserTicket ttuserTicket.tusrid ttuserTicket.tusrTicketId ttuserTicket.tassigne ttuserTicket.tstartdate ttuserTicket.tenddate ttuserTicket.tticketId ttuserTicket.tticketStatus ttuserTicket.tTicketDiscription ttuserTicket.tcategory   
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwuserTicket ttuserTicket.tticketStatus   
&Scoped-define ENABLED-TABLES-IN-QUERY-brwuserTicket ttuserTicket
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brwuserTicket ttuserTicket
&Scoped-define SELF-NAME brwuserTicket
&Scoped-define QUERY-STRING-brwuserTicket FOR EACH ttuserTicket
&Scoped-define OPEN-QUERY-brwuserTicket OPEN QUERY quserTicket FOR EACH ttuserTicket.
&Scoped-define TABLES-IN-QUERY-brwuserTicket ttuserTicket
&Scoped-define FIRST-TABLE-IN-QUERY-brwuserTicket ttuserTicket


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwuserTicket}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-4 btnAdTr filUserId ~
btnSearch btnAdSearch BtnUpUsr fillFName filLName filEmail btnDeleteUsr ~
filPhone btnreport btnUpdateusr bselect bdeselect brwuserTicket ~
btnAddUsrTicket btnUpdate btnDelete 
&Scoped-Define DISPLAYED-OBJECTS filUserId fillFName filLName filEmail ~
filPhone bselect bdeselect 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddUsrTicket 
    LABEL "Add Ticket" 
    SIZE 17 BY 1.19.

DEFINE BUTTON btnAdSearch 
    LABEL "Advance Search" 
    SIZE 20 BY 1.24.

DEFINE BUTTON btnAdTr 
    LABEL "Add User" 
    SIZE 17 BY 1.19.

DEFINE BUTTON btnDelete 
    LABEL "Delete Ticket" 
    SIZE 17 BY 1.19.

DEFINE BUTTON btnDeleteUsr 
    LABEL "Delete" 
    SIZE 17 BY 1.19.

DEFINE BUTTON btnreport 
    LABEL "Report" 
    SIZE 16 BY 1.19.

DEFINE BUTTON btnSearch 
    LABEL "Search" 
    SIZE 20 BY 1.24.

DEFINE BUTTON btnUpdate 
    LABEL "Update Ticket" 
    SIZE 17 BY 1.19.

DEFINE BUTTON btnUpdateusr 
    LABEL "Update ticket" 
    SIZE 17 BY 1.19.

DEFINE BUTTON BtnUpUsr 
    LABEL "Update User" 
    SIZE 17 BY 1.19.

DEFINE VARIABLE filEmail  AS CHARACTER FORMAT "X(256)":U 
    LABEL "Email ID" 
    VIEW-AS FILL-IN 
    SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE fillFName AS CHARACTER FORMAT "X(256)":U 
    LABEL "First Name" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filLName  AS CHARACTER FORMAT "X(256)":U 
    LABEL "Last Name" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filPhone  AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
    LABEL "Phone" 
    VIEW-AS FILL-IN 
    SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filUserId AS CHARACTER FORMAT "X(256)":U 
    LABEL "User ID" 
    VIEW-AS FILL-IN 
    SIZE 13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 120 BY 6.19.

DEFINE RECTANGLE RECT-2
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 21 BY 6.19.

DEFINE RECTANGLE RECT-4
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 20 BY 6.19.

DEFINE VARIABLE bdeselect AS LOGICAL INITIAL no 
    LABEL "Deselect" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1.19 NO-UNDO.

DEFINE VARIABLE bselect   AS LOGICAL INITIAL no 
    LABEL "Select" 
    VIEW-AS TOGGLE-BOX
    SIZE 16 BY 1.19 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwuserTicket FOR 
    ttuserTicket SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwuserTicket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwuserTicket C-Win _FREEFORM
    QUERY brwuserTicket DISPLAY
    ttuserTicket.tusrid LABEL "Userid"
    ttuserTicket.tusrTicketId LABEL "UserTicketID"
    ttuserTicket.tassigne LABEL "Assigne" FORMAT "x(10)" 
    ttuserTicket.tstartdate LABEL "Start date" 
    ttuserTicket.tenddate LABEL "End date"
    ttuserTicket.tticketId LABEL "Ticket ID"
    ttuserTicket.tticketStatus column-LABEL "Ticketstatus" VIEW-AS COMBO-BOX LIST-ITEMS "Open,Pending,Hold,Progress,Close" 
    ttuserTicket.tTicketDiscription LABEL "Ticket Description" FORMAT "x(20)"
    ttuserTicket.tcategory label "category" FORMAT "x(40)"
    
    ENABLE ttuserTicket.tticketStatus
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 120 BY 5.95 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    btnAdTr AT ROW 3.38 COL 128 WIDGET-ID 204
    filUserId AT ROW 3.62 COL 15 COLON-ALIGNED WIDGET-ID 2
    btnSearch AT ROW 3.62 COL 51 WIDGET-ID 12
    btnAdSearch AT ROW 3.62 COL 83 WIDGET-ID 14
    BtnUpUsr AT ROW 5.29 COL 128 WIDGET-ID 208
    fillFName AT ROW 5.76 COL 15 COLON-ALIGNED WIDGET-ID 4
    filLName AT ROW 5.76 COL 49 COLON-ALIGNED WIDGET-ID 6
    filEmail AT ROW 5.76 COL 81 COLON-ALIGNED WIDGET-ID 8
    btnDeleteUsr AT ROW 7.19 COL 128 WIDGET-ID 26
    filPhone AT ROW 7.67 COL 15 COLON-ALIGNED WIDGET-ID 10
    btnreport AT ROW 9.1 COL 105 WIDGET-ID 218
    btnUpdateusr AT ROW 9.1 COL 129 WIDGET-ID 202
    bselect AT ROW 10.05 COL 7 WIDGET-ID 212
    bdeselect AT ROW 10.05 COL 24 WIDGET-ID 214
    brwuserTicket AT ROW 11.71 COL 5 WIDGET-ID 200
    btnAddUsrTicket AT ROW 12.19 COL 128 WIDGET-ID 18
    btnUpdate AT ROW 13.86 COL 128 WIDGET-ID 20
    btnDelete AT ROW 15.52 COL 128 WIDGET-ID 22
    "USER TICKET INFORMATION" VIEW-AS TEXT
    SIZE 35 BY 1.19 AT ROW 9.33 COL 47 WIDGET-ID 206
    FONT 6
    "USER INFORMATION" VIEW-AS TEXT
    SIZE 27 BY 1.19 AT ROW 1.48 COL 78 RIGHT-ALIGNED WIDGET-ID 200
    FGCOLOR 0 FONT 6
    RECT-1 AT ROW 2.67 COL 5 WIDGET-ID 16
    RECT-2 AT ROW 2.67 COL 126 WIDGET-ID 28
    RECT-4 AT ROW 11.48 COL 127 WIDGET-ID 216
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1.05
    SIZE 150 BY 17.19 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Ticket Information"
        HEIGHT             = 17.48
        WIDTH              = 151
        MAX-HEIGHT         = 17.48
        MAX-WIDTH          = 151
        VIRTUAL-HEIGHT     = 17.48
        VIRTUAL-WIDTH      = 151
        RESIZE             = yes
        SCROLL-BARS        = no
        STATUS-AREA        = no
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = yes
        THREE-D            = yes
        MESSAGE-AREA       = no
        SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwuserTicket bdeselect DEFAULT-FRAME */
/* SETTINGS FOR TEXT-LITERAL "USER INFORMATION"
          SIZE 27 BY 1.19 AT ROW 1.48 COL 78 RIGHT-ALIGNED              */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwuserTicket
/* Query rebuild information for BROWSE brwuserTicket
     _START_FREEFORM
OPEN QUERY quserTicket FOR EACH ttuserTicket.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwuserTicket */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ticket Information */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON MOUSE-SELECT-CLICK OF C-Win /* Ticket Information */
    DO:
        FOR EACH ttuserTicket NO-LOCK BY ttuserTicket.tticketId DESCENDING:
        /* Do something */
        END.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ticket Information */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bdeselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bdeselect C-Win
ON VALUE-CHANGED OF bdeselect IN FRAME DEFAULT-FRAME /* Deselect */
    DO:
        brwuserTicket:DESELECT-ROWS ().
        bselect:CHECKED = FALSE.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwuserTicket
&Scoped-define SELF-NAME brwuserTicket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwuserTicket C-Win
ON MOUSE-SELECT-DBLCLICK OF brwuserTicket IN FRAME DEFAULT-FRAME
    DO:
        DEFINE VARIABLE hSortColumn AS WIDGET-HANDLE.

        hSortColumn = brwuserTicket:CURRENT-COLUMN.

        CASE hSortColumn:LABEL:
            WHEN "UserTicketID" THEN
                OPEN QUERY brwuserTicket FOR each ttuserTicket by ttuserTicket.tusrTicketId.
            WHEN "UserTicketID" THEN
                OPEN QUERY brwuserTicket FOR EACH ttuserTicket BY ttuserTicket.tusrTicketId.
        /*                FOR EACH ttuserTicket NO-LOCK BY ttuserTicket.tticketId DESCENDING:*/
        /*  /* Do something */                                                               */
        /*    END.                                                                           */
    
        END CASE.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwuserTicket C-Win
ON ROW-DISPLAY OF brwuserTicket IN FRAME DEFAULT-FRAME
    DO:
    

        IF ttuserTicket.tticketStatus = "Hold" THEN
       
        DO:
            ttuserTicket.tticketStatus:BGCOLOR in  browse brwuserTicket = 14.
            ttuserTicket.tassigne:BGCOLOR in  browse brwuserTicket = 14.          
            ttuserTicket.tenddate:BGCOLOR in  browse brwuserTicket = 14.
            ttuserTicket.tstartdate:BGCOLOR in  browse brwuserTicket = 14.
            ttuserTicket.tTicketDiscription:BGCOLOR in  browse brwuserTicket = 14.
            ttuserTicket.tusrTicketId:BGCOLOR in  browse brwuserTicket = 14.
            ttuserTicket.tusrid:BGCOLOR in  browse brwuserTicket = 14.
        END.
        IF ttuserTicket.tticketStatus = "Open" THEN
        DO:
            ttuserTicket.tticketStatus:BGCOLOR in  browse brwuserTicket = 12.
            ttuserTicket.tassigne:BGCOLOR in  browse brwuserTicket = 12.          
            ttuserTicket.tenddate:BGCOLOR in  browse brwuserTicket = 12.
            ttuserTicket.tstartdate:BGCOLOR in  browse brwuserTicket = 12.
            ttuserTicket.tTicketDiscription:BGCOLOR in  browse brwuserTicket = 12.
            ttuserTicket.tusrTicketId:BGCOLOR in  browse brwuserTicket = 12.
            ttuserTicket.tusrid:BGCOLOR in  browse brwuserTicket = 12.
        END.
        IF ttuserTicket.tticketStatus = "Close" THEN
        DO:
            ttuserTicket.tticketStatus:BGCOLOR in  browse brwuserTicket = 13.
            ttuserTicket.tassigne:BGCOLOR in  browse brwuserTicket = 13.          
            ttuserTicket.tenddate:BGCOLOR in  browse brwuserTicket = 13.
            ttuserTicket.tstartdate:BGCOLOR in  browse brwuserTicket = 13.
            ttuserTicket.tTicketDiscription:BGCOLOR in  browse brwuserTicket = 13.
            ttuserTicket.tusrTicketId:BGCOLOR in  browse brwuserTicket = 13.
            ttuserTicket.tusrid:BGCOLOR in  browse brwuserTicket = 13.
        END.
        IF ttuserTicket.tticketStatus = "Pending" THEN
        DO:
            ttuserTicket.tticketStatus:BGCOLOR in  browse brwuserTicket = 11.
            ttuserTicket.tassigne:BGCOLOR in  browse brwuserTicket = 11.          
            ttuserTicket.tenddate:BGCOLOR in  browse brwuserTicket = 11.
            ttuserTicket.tstartdate:BGCOLOR in  browse brwuserTicket = 11.
            ttuserTicket.tTicketDiscription:BGCOLOR in  browse brwuserTicket = 11.
            ttuserTicket.tusrTicketId:BGCOLOR in  browse brwuserTicket = 11.
            ttuserTicket.tusrid:BGCOLOR in  browse brwuserTicket = 11.
        END.
        
                                                
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwuserTicket C-Win
ON START-SEARCH OF brwuserTicket IN FRAME DEFAULT-FRAME
    DO:
       
        DEFINE VARIABLE hSortColumn AS WIDGET-HANDLE.

        hSortColumn = brwuserTicket:CURRENT-COLUMN.

        CASE hSortColumn:LABEL:
            WHEN "UserTicketID" THEN
                OPEN QUERY brwuserTicket FOR each ttuserTicket by ttuserTicket.tusrTicketId DESCENDING.
            WHEN "UserTicketID" THEN
                OPEN QUERY brwuserTicket FOR EACH ttuserTicket BY ttuserTicket.tusrTicketId DESCENDING.
        /*                FOR EACH ttuserTicket NO-LOCK BY ttuserTicket.tticketId DESCENDING:*/
        /*  /* Do something */                                                               */
        /*    END.                                                                           */
    
        END CASE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bselect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bselect C-Win
ON VALUE-CHANGED OF bselect IN FRAME DEFAULT-FRAME /* Select */
    DO:
       
        do:
            brwuserTicket:SELECT-ALL ().
            bdeselect:CHECKED = FALSE.  
        end.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAddUsrTicket
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddUsrTicket C-Win
ON CHOOSE OF btnAddUsrTicket IN FRAME DEFAULT-FRAME /* Add Ticket */
    DO:
        DEFINE VARIABLE ticketId AS INTEGER.
        DEFINE VARIABLE usrid    AS INTEGER.
        if filUserId:SCREEN-VALUE = "" OR filUserId:SCREEN-VALUE = "0" THEN
        do:
            MESSAGE "selct the record"
                VIEW-AS ALERT-BOX.
        end.
        else
        do:
            usrid = int(filUserId:SCREEN-VALUE).   
            ticketId = NEXT-VALUE (nestUsrTicketId).
            run ui/AddTicket.w(INPUT usrid,INPUT ticketId,output table ttuserTicket).
            
            APPLY "choose" to btnSearch.
        end.
        
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
        END CATCH.

        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdSearch C-Win
ON CHOOSE OF btnAdSearch IN FRAME DEFAULT-FRAME /* Advance Search */
    DO: 
        DEFINE VARIABLE inum AS INTEGER.
        RUN ui/AdvanceSearch.w(OUTPUT inum).
        filUserId:SCREEN-VALUE = string(inum).
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
        END CATCH.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAdTr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdTr C-Win
ON CHOOSE OF btnAdTr IN FRAME DEFAULT-FRAME /* Add User */
    DO:
        DEFINE VARIABLE inum AS INTEGER.
        /*  MESSAGE inum                                  */
        /*  VIEW-AS ALERT-BOX.                            */
        filUserId:SCREEN-VALUE = string(inum).
        /*        MESSAGE inum                            */
        /*  VIEW-AS ALERT-BOX.                            */
        RUN ui/AddUser.w(OUTPUT inum,OUTPUT TABLE ttuser).
        APPLY "choose" to btnSearch.
        
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
            ASSIGN
                fillFName:SCREEN-VALUE = ""
                filLName:SCREEN-VALUE  = ""
                filEmail:SCREEN-VALUE  = ""
                filPhone:SCREEN-VALUE  = "".
        END CATCH. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete Ticket */
    DO:
        DEFINE VARIABLE objdeleteUsrTicket AS businesslogic.ItRequest.
        objdeleteUsrTicket = NEW businesslogic.ItRequest( ). 
        objdeleteUsrTicket:iusrTicketId = ttuserTicket.tusrTicketId.
        objdeleteUsrTicket:deleteUserTicket().
        APPLY "choose" TO btnSearch.  
        CATCH e AS Progress.Lang.Error:
            MESSAGE "select the record"
                VIEW-AS ALERT-BOX. 
        /*            MESSAGE e:GetMessage(1)*/
        /*                VIEW-AS ALERT-BOX. */
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(objdeleteUsrTicket)THEN DELETE OBJECT objdeleteUsrTicket NO-ERROR.

        END FINALLY.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteUsr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteUsr C-Win
ON CHOOSE OF btnDeleteUsr IN FRAME DEFAULT-FRAME /* Delete */
    DO:
        DEFINE VARIABLE objdeleteUsrTicket AS businesslogic.ItRequest.
        objdeleteUsrTicket = NEW businesslogic.ItRequest( ). 
        objdeleteUsrTicket:usrNum = ttUser.ttusrid.
        EMPTY TEMP-TABLE ttUser.
        objdeleteUsrTicket:deleteUser().
        APPLY "choose" to btnSearch.
        CATCH e AS Progress.Lang.Error:
            MESSAGE "select the record"
                VIEW-AS ALERT-BOX. 
        /*            MESSAGE e:GetMessage(1)*/
        /*                VIEW-AS ALERT-BOX. */
        END CATCH.

        FINALLY:
            IF VALID-OBJECT(objdeleteUsrTicket)THEN DELETE OBJECT objdeleteUsrTicket NO-ERROR.

        END FINALLY.  
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnreport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnreport C-Win
ON CHOOSE OF btnreport IN FRAME DEFAULT-FRAME /* Report */
    DO:
        DEFINE VARIABLE oreport As businesslogic.ItRequest NO-UNDO.
        oreport = NEW businesslogic.ItRequest().
        oreport:reportGen().
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch C-Win
ON CHOOSE OF btnSearch IN FRAME DEFAULT-FRAME /* Search */
    DO:
        
        DEFINE VARIABLE oCustomer AS businesslogic.ItRequest NO-UNDO.  
        oCustomer = NEW businesslogic.ItRequest().
        /*        btnUpdateusr:SENSITIVE = false.*/
        
        if(filUserId:SCREEN-VALUE ="")OR(filUserId:SCREEN-VALUE = "0") THEN
        DO:
            MESSAGE "plz enter correct custid"
                VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO filUserId.
            ASSIGN
            
                
                fillFName:SCREEN-VALUE = ""
                filLName:SCREEN-VALUE  = ""
                filEmail:SCREEN-VALUE  = ""
                filPhone:SCREEN-VALUE  = "".
            
        END.
        
        else
        DO:
            
            oCustomer:usrNum = INTEGER(filUserId:SCREEN-VALUE).
            EMPTY TEMP-TABLE ttUser.
            
            oCustomer:userFetch(INPUT TEMP-TABLE ttUser:DEFAULT-BUFFER-HANDLE).
            
           
            FIND FIRST ttUser where ttUser.ttusrid = oCustomer:usrNum NO-LOCK NO-ERROR.  
            if AVAILABLE ttUser THEN
            DO:
                ASSIGN
                
                    btnUpdate:SENSITIVE       = true
                    btnAddUsrTicket:SENSITIVE = true
                    btnDelete:SENSITIVE       = true
                    
                    filUserId:SCREEN-VALUE    = string(ttUser.ttusrid)
                    fillFName:SCREEN-VALUE    = string(ttUser.ttfirstName)
                    filLName:SCREEN-VALUE     = string(ttUser.ttlastName)
                    filEmail:SCREEN-VALUE     = string(ttUser.ttemaiid)
                    filPhone:SCREEN-VALUE     = string(ttUser.ttphone).
                    
            END.
            else
            do:
                ASSIGN
                    filUserId:SCREEN-VALUE = ""
                    fillFName:SCREEN-VALUE = ""
                    filLName:SCREEN-VALUE  = ""
                    filEmail:SCREEN-VALUE  = ""
                    filPhone:SCREEN-VALUE  = "".
            end.
                
            
        END.
       
        oCustomer:usrNum = INTEGER(filUserId:SCREEN-VALUE).
        EMPTY TEMP-TABLE ttuserTicket.
       
        oCustomer:userTicketfetch(INPUT TEMP-TABLE ttuserTicket:DEFAULT-BUFFER-HANDLE).
        
        OPEN QUERY brwuserTicket FOR EACH ttuserTicket NO-LOCK. 
         
            
        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.
            ASSIGN
                filUserId:SCREEN-VALUE = ""
                fillFName:SCREEN-VALUE = ""
                filLName:SCREEN-VALUE  = ""
                filEmail:SCREEN-VALUE  = ""
                filPhone:SCREEN-VALUE  = "".
            EMPTY TEMP-TABLE ttuserTicket.
            OPEN QUERY brwuserTicket FOR EACH ttuserTicket NO-LOCK. 
            
                    
        END CATCH.
        
        FINALLY:
            IF VALID-OBJECT(oCustomer)THEN DELETE OBJECT oCustomer NO-ERROR.

        END FINALLY.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdate C-Win
ON CHOOSE OF btnUpdate IN FRAME DEFAULT-FRAME /* Update Ticket */
    DO:
        DEFINE VARIABLE iuserTicket AS INTEGER NO-UNDO.
          
        iuserTicket = ttuserTicket.tusrTicketId.
        
        RUN UI/UpdateUserTicket.w(INPUT iuserTicket,INPUT TABLE ttuserTicket).
        btnUpdate:SENSITIVE = true.
        
        APPLY "choose" to btnSearch.
        
        
        CATCH e AS Progress.Lang.Error:
            MESSAGE "select the record"
                VIEW-AS ALERT-BOX.         
            
                
        END CATCH.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUpdateusr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUpdateusr C-Win
ON CHOOSE OF btnUpdateusr IN FRAME DEFAULT-FRAME /* Update ticket */
    DO:
        /*        DEFINE VARIABLE objusrTicket AS businesslogic.ItRequest.                      */
        /*        DEFINE VARIABLE objval       AS businesslogic.Validation.                     */
        /*        DEFINE VARIABLE lValidName   AS LOGICAL                  NO-UNDO.             */
        /*        DEFINE VARIABLE lValidlName  AS LOGICAL                  NO-UNDO.             */
        /*        DEFINE VARIABLE lresult      AS LOGICAL                  NO-UNDO.             */
        /*        DEFINE VARIABLE lValiPhone   AS LOGICAL                  NO-UNDO.             */
        /*        DEFINE VARIABLE lvalidphone  AS LOGICAL                  NO-UNDO.             */
        /*        DEFINE VARIABLE lValidEmail  AS LOGICAL                  NO-UNDO.             */
        /*        objval = new businesslogic.Validation().                                      */
        /*                                                                                      */
        /*        objusrTicket = NEW businesslogic.ItRequest().                                 */
        /*                                                                                      */
        /*        objusrTicket:usrNum = int(filUserId:SCREEN-VALUE).                            */
        /*        FIND FIRST ttUser WHERE ttUser.ttusrid = objusrTicket:usrNum NO-LOCK NO-ERROR.*/
        /*        MESSAGE ".w before if"                                                        */
        /*            VIEW-AS ALERT-BOX.                                                        */
        /*        IF AVAILABLE ttUser THEN                                                      */
        /*        DO:                                                                           */
        /*            MESSAGE ".w after if"                                                     */
        /*                VIEW-AS ALERT-BOX.                                                    */
        /*            ASSIGN                                                                    */
        /*                ttUser.ttusrid     = int(filUserId:SCREEN-VALUE)                      */
        /*                ttUser.ttfirstName = fillFName:SCREEN-VALUE                           */
        /*                ttUser.ttlastName  = filLName:SCREEN-VALUE                            */
        /*                ttUser.ttphone     = filPhone:SCREEN-VALUE                            */
        /*                ttUser.ttemaiid    = filEmail:SCREEN-VALUE.                           */
        /*                                                                                      */
        /*            MESSAGE ttUser.ttusrid ".w end"                                           */
        /*                VIEW-AS ALERT-BOX.                                                    */
        /*                                                                                      */
        /*                                                                                      */
        /*        END.                                                                          */
        /*        objval:valilname(input filLName:SCREEN-VALUE,OUTPUT lValidlName).             */
        /*        objval:fnameValidation(INPUT fillFName:SCREEN-VALUE, output lValidName).      */
        /*        objval:Phonevalidate(input filPhone:SCREEN-VALUE,OUTPUT lValiPhone).          */
        /*       //objval:validEmail(input filEmail:SCREEN-VALUE, OUTPUT lValidName).           */
        /*                                                                                      */
        /*        if  lValidName = TRUE  AND lValidlName = TRUE AND lValiPhone = TRUE  THEN     */
        /*        DO:                                                                           */
        /*            objusrTicket:updateUser(INPUT TABLE ttUser).                              */
        /*        END.                                                                          */
        /*        CATCH e AS Progress.Lang.Error:                                               */
        /*            MESSAGE e:GetMessage(1)                                                   */
        /*                VIEW-AS ALERT-BOX.                                                    */
        /*        END CATCH.                                                                    */
        /*                                                                                      */
        /*        FINALLY:                                                                      */
        /*            IF VALID-OBJECT(objusrTicket)THEN DELETE OBJECT objusrTicket NO-ERROR.    */
        /*                                                                                      */
        /*        END FINALLY.                                                                  */
        DEFINE VARIABLE objusrTicket AS businesslogic.ItRequest.
        DEFINE VARIABLE objval       AS businesslogic.Validation.
        objval = NEW businesslogic.Validation().
        objusrTicket = NEW businesslogic.ItRequest().
        
        objusrTicket:iusrTicketId = ttuserTicket.tusrTicketId.
        //FIND FIRST ttuserTicket WHERE ttuserTicket.tusrTicketId = objusrTicket:iusrTicketId NO-LOCK NO-ERROR.
        //FIND first ttuserTicket WHERE ttuserTicket.tusrTicketId = objusrTicket:iusrTicketId NO-LOCK NO-ERROR.
        //IF AVAILABLE ttuserTicket THEN 
        
        for each ttuserTicket WHERE ttuserTicket.tusrTicketId = objusrTicket:iusrTicketId EXCLUSIVE-LOCK:
            DO:
            /*            ASSIGN                                                         */
            /*                ttuserTicket.tticketStatus                                 */
            /*                ttuserTicket.tenddate      = date(filEndDate:SCREEN-VALUE).*/
                                
            END.
        END.
        objusrTicket:UpdateUsrTicket(INPUT TABLE ttuserTicket).   
        APPLY "choose" to btnSearch.

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


&Scoped-define SELF-NAME BtnUpUsr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnUpUsr C-Win
ON CHOOSE OF BtnUpUsr IN FRAME DEFAULT-FRAME /* Update User */
    DO:
        DEFINE VARIABLE iUsrNum AS INTEGER NO-UNDO.
        iUsrNum = ttUser.ttusrid.
        run ui/UpdateUser.w(INPUT iUsrNum ,INPUT TABLE ttUser).
        APPLY "choose" to btnSearch.
        /*        BtnUpUsr:SENSITIVE = true.*/
        CATCH e AS Progress.Lang.Error:
            MESSAGE "select the record"
                VIEW-AS ALERT-BOX. 
        /*            MESSAGE e:GetMessage(1)*/
        /*                VIEW-AS ALERT-BOX. */
        END CATCH.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    btnUpdate:SENSITIVE = false.
    btnAddUsrTicket:SENSITIVE = false.
    btnDelete:SENSITIVE = false.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY filUserId fillFName filLName filEmail filPhone bselect bdeselect 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    ENABLE RECT-1 RECT-2 RECT-4 btnAdTr filUserId btnSearch btnAdSearch BtnUpUsr 
        fillFName filLName filEmail btnDeleteUsr filPhone btnreport 
        btnUpdateusr bselect bdeselect brwuserTicket btnAddUsrTicket btnUpdate 
        btnDelete 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

