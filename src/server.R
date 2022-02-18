function(input, output, session) {
  
  ##############################
  # BASE : APPLICATION STARTUP #
  ##############################
  
  # Specify session timeout
  observeEvent(input$timeout, {
    generate_message(message = paste0("Session timed out due to inactivity:<br/>", Sys.time()))
    session$close()
  })
  
  # Import data necessary for user authentication
  app = reactiveValues(user_pk = NULL)
  db = reactiveValues(users = load_table("users"))
  
  # If user's browser has already stored a cookie, log user back in
  observe({
    js$getcookie()
    if (!is.null(input$jscookie)) {
      app$user_pk = verify_token(jwt = input$jscookie, user_pks = db$users$user_pk)
    }
  })
  
  # Application skeleton
  output$body = renderUI({
    if (!is.null(app$user_pk)) uiOutput(outputId = app$tab) else login_page
  })
  
  # Login page to accept user credentials
  login_page = tags$div(
    id = "login_page",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    wellPanel(
      tags$h2("PPE Exchange",
              class = "text-center",
              style = "padding-top: 0;"),
      tags$br(),
      tags$br(),
      textInput(inputId = "email",
                placeholder = "@your_company.com",
                label = tagList(icon("user"), "Email Address")),
      passwordInput(inputId = "password",
                    label = tagList(icon("key"), "Password")),
      checkboxInput(inputId = "store_token",
                    label = sprintf(
                      "Enable auto-login for %s days (do NOT do this on a shared device)",
                      options()$expiry
                    )),
      tags$br(),
      tags$div(style = "text-align: center;",
               actionButton(inputId = "login",
                            label = "Log In",
                            icon = icon("sign-in-alt"),
                            width = "30%"),
               hidden(tags$div(id = "access_denied",
                               tags$p("Invalid email address or password.",
                                      style = "color: red; padding-top: 5px;",
                                      class = "text-center"))))
    )
  )
  
  
  ##############################
  # BASE : USER AUTHENTICATION #
  ##############################
  
  # Determine whether the user has provided a correct set of credentials
  observeEvent(input$login, {
    email = trimws(tolower(isolate(input$email)))
    password = isolate(input$password)
    hash_password = db$users$password[db$users$email == email]
    if (email %in% db$users$email && password_verify(hash_password, password)) {
      app$user_pk = db$users$user_pk[db$users$email == email]
      if (input$store_token) {
        token = issue_token(user_pk = app$user_pk)
        js$setcookie(token)
      }
    } else {
      toggle(id = "access_denied", anim = TRUE, time = 1, animType = "fade")
      delay(ms = 3000,
            expr = toggle(id = "access_denied", anim = TRUE, time = 1, animType = "fade"))
    }
  }, ignoreInit = TRUE)
  
  # Once a user has authenticated, log that and allow them access to the data
  observeEvent(app$user_pk, {
    if (!is.null(app$user_pk)) {
      run_query(query = sprintf(
        "UPDATE users SET last_login = NOW() WHERE user_pk = %s;",
        app$user_pk
      ))
      app$tab = "exchange_forum"
      db$clinics = load_table("clinics")
      db$equipment = load_table("equipment")
      db$groups = load_table("groups")
      db$group_clinic_interface = load_table("group_clinic_interface")
      db$posts = load_table("posts")
      db$user_clinic_interface = load_table("user_clinic_interface")
    }
  })
  
  
  #######################
  # BASE : CREATE VIEWS #
  #######################
  
  # Create a view of posts
  vw_posts = reactive({
    x = merge(db$posts, db$equipment[db$equipment$is_active,])
    x = merge(x, db$users)
    x = merge(x, db$clinics)
    x = merge(x, db$users,
              by.x = "created_by", by.y = "user_pk",
              suffixes = c("", "_cr"))
    x = merge(x, db$users,
              by.x = "modified_by", by.y = "user_pk",
              all.x = TRUE,
              suffixes = c("", "_mo"))
    x$created_by = NULL
    colnames(x)[colnames(x) == "user_desc_cr"] = "created_by"
    x$created_on = format(x$created_on, format = "%F %r")
    x$modified_by = NULL
    colnames(x)[colnames(x) == "user_desc_mo"] = "modified_by"
    x$modified_by[is.na(x$modified_by)] = ""
    x$modified_on = format(x$modified_on, format = "%F %r")
    x$modified_on[is.na(x$modified_on)] = ""
    x = x[if (input$show_active_only) x$status %in% c("Available", "Unfilled") else TRUE,]
    x[order(x$post_pk),]
  })
  
  # Create a view of users and the clinics they have access to
  vw_user_clinics = reactive({
    x = merge(db$users, db$user_clinic_interface, all.x = TRUE)
    x[is.na(x$clinic_pk), c("clinic_pk", "is_site_admin")] = FALSE
    x = merge(x, db$clinics, all.x = TRUE)
    x$contact_info = gsub(
      pattern = "NA<br/>",
      replacement = "",
      x = paste(
        x$user_desc,
        x$clinic_desc,
        x$address1,
        x$address2,
        paste("San Francisco, CA", x$zip),
        x$phone,
        x$email,
        "<br/>",
        sep = "<br/>"
      )
    )
    x[order(x$user_desc, x$is_site_admin),]
  })
  
  # Create a view summarizing the active user's permissions
  user = eventReactive(vw_user_clinics(), {
    if (!is.null(app$user_pk)) 
      vw_user_clinics()[vw_user_clinics()$user_pk %in% app$user_pk,]
  })
  
  ########################
  # BASE: NAVIGATION BAR #
  ########################
  
  # Top-level navigation bar
  output$navigation_bar = renderMenu({
    # req(app$authenticated)
    req(!is.null(app$user_pk))
    tags$ul(
      actionLink(inputId = "exchange_forum",
                 label = "Exchange Forum",
                 icon = icon("comments"),
                 class = "link link-active"),
      actionLink(inputId = "settings",
                 label = "Settings",
                 icon = icon("cog"),
                 class = "link"),
      actionLink(inputId = "status_report",
                 label = "Status Report",
                 icon = icon("chart-bar"),
                 class = "link"),
      actionLink(inputId = "logout",
                 label = "Log Out",
                 icon = icon("sign-out-alt"),
                 class = "link")
    )
  })
  
  # Button selection logic
  observeEvent(input$exchange_forum, {
    app$tab = "exchange_forum"
    addClass(id = "exchange_forum", class = "link-active")
    removeClass(id = "settings", class = "link-active")
    removeClass(id = "status_report", class = "link-active")
  })
  observeEvent(input$settings, {
    app$tab = "settings"
    addClass(id = "settings", class = "link-active")
    removeClass(id = "exchange_forum", class = "link-active")
    removeClass(id = "status_report", class = "link-active")
  })
  observeEvent(input$status_report, {
    app$tab = "status_report"
    addClass(id = "status_report", class = "link-active")
    removeClass(id = "exchange_forum", class = "link-active")
    removeClass(id = "settings", class = "link-active")
  })
  observeEvent(input$logout, {
    js$rmcookie()
    app$user_pk = NULL
    session$reload()
  })
  
  
  ###################################
  # EXCHANGE FORUM : MAIN INTERFACE #
  ###################################
  
  # Create an interactive data table to show all PPE posts made
  output$ir_posts = renderDataTable({
    req(app$tab == "exchange_forum")
    datatable(
      data = vw_posts()[, c("type", "equipment_desc", "units", "clinic_desc", "user_desc", "status")],
      colnames = c("Type", "Equipment", "Units", "Clinic Name", "Contact Name", "Status"),
      escape = FALSE,
      filter = "top",
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "8%"),
                                       list(targets = 1, width = "28%"),
                                       list(targets = 2, width = "8%"),
                                       list(targets = 3, width = "34%"),
                                       list(targets = 4, width = "14%"),
                                       list(targets = 5, width = "8%")),
                     dom = "it",
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the table of PPE posts submitted
  output$exchange_forum = renderUI({
    fixedPage(
      h3("Exchange Forum"),
      helpText(paste(
        "To create a post, please fill out a form using the button to the right,",
        "and your data will be populated to the list."
      )),
      helpText(paste(
        "If your unit can fill a request or would like to accept an offer,",
        "please feel free to contact the poster directly.",
        "You may view their contact information by clicking on the post row."
      )),
      helpText(paste(
        "When a post is completed or cancelled,",
        "we ask the poster to please update the status of the post accordingly.",
        "You may do this also by clicking the post row."
      )),
      fixedRow(column(width = 12, tags$hr())),
      fixedRow(
        column(width = 10,
               checkboxInput(inputId = "show_active_only",
                             label = "Show active posts only",
                             value = TRUE),
               style = "display: inline-block;"),
        column(width = 2,
               actionButton(inputId = "add_post",
                            label = "Create New Post",
                            icon = icon("plus")))
      ),
      tags$br(),
      dataTableOutput(outputId = "ir_posts"),
    )
  })
  
  # Generate a contact info summary for the selected post
  output$contact_info = renderUI({
    if (app$current_interact == "add") {
      clinic_pk = input[[paste0("clinic_pk_add", input$add_post)]]
      user_pk = input[[paste0("user_pk_add", input$add_post)]]
    } else if (post()$clinic_pk %in% user()$clinic_pk) {
      clinic_pk = input[[paste0("clinic_pk_mod", post()$post_pk)]]
      user_pk = input[[paste0("user_pk_mod", post()$post_pk)]]
    } else {
      clinic_pk = post()$clinic_pk
      user_pk = post()$user_pk
    }
    user_clinic_record = vw_user_clinics()$clinic_pk %in% clinic_pk &
      vw_user_clinics()$user_pk %in% user_pk
    HTML(if (any(user_clinic_record)) {
      vw_user_clinics()$contact_info[user_clinic_record]
    } else {
      "<div style='color:red'>Invalid contact information</div><br/>"
    })
  })
  
  
  ####################################
  # EXCHANGE FORUM : CREATE NEW POST #
  ####################################
  
  # Update the list contact persons in the "Create New Post" modal dialog
  observeEvent(input[[paste0("clinic_pk_add", input$add_post)]], {
    updateSelectInput(
      session,
      inputId = paste0("user_pk_add", input$add_post),
      choices = lookup(vw_user_clinics()[
        vw_user_clinics()$clinic_pk %in% input[[paste0("clinic_pk_add", input$add_post)]],
        c("user_pk", "user_desc")
        ]),
      selected = user()$user_pk
    )
  })
  
  # Generate a modal dialog to accept user input regarding new post
  observeEvent(input$add_post, {
    app$current_interact = "add"
    if (is.na(user()$user_clinic_pk[1])) {
      generate_message(message = paste0(
        "You must be on a clinic's team to post.<br/>",
        "Find the clinic list at [Settings > Edit Clinics] ",
        "and contact a clinic's admin(s) to be added."
      ))
    } else {
      showModal(modalDialog(
        title = "Create New Post",
        fluidPage(
          fluidRow(
            column(width = 3,
                   selectInput(inputId = paste0("type_add", input$add_post),
                               label = "Post Type",
                               choices = c("Request", "Offer"))),
            column(width = 7,
                   selectInput(inputId = paste0("equipment_pk_add", input$add_post),
                               label = "Equipment",
                               choices = lookup(db$equipment[db$equipment$is_active,]))),
            column(width = 2,
                   numericInput(inputId = paste0("units_add", input$add_post),
                                label = "Units",
                                value = 1,
                                min = 1))
          ),
          fluidRow(
            column(width = 8,
                   selectInput(inputId = paste0("clinic_pk_add", input$add_post),
                               label = "Clinic Name",
                               choices = lookup(user()[, c("clinic_pk", "clinic_desc")]))),
            column(width = 4,
                   selectInput(inputId = paste0("user_pk_add", input$add_post),
                               label = "Contact Name",
                               choices = numeric(0)))
          ),
          fluidRow(
            column(width = 3,
                   HTML("<strong>Please contact:</strong>")),
            column(width = 9,
                   htmlOutput(outputId = "contact_info"))
          ),
          fluidRow(column(width = 12,
                          textAreaInput2(inputId = paste0("note_add", input$add_post),
                                         label = "Note",
                                         rows = 2,
                                         placeholder = "No PHI please.")))
        ),
        easyClose = TRUE,
        footer = fluidPage(
          fluidRow(column(width = 4,
                          actionButton(inputId = "save_add_post",
                                       label = "Create Post",
                                       icon = icon("plus")),
                          offset = 8)),
          tags$br()
        ),
        size = "m"
      ))
    }
  }, ignoreInit = TRUE)
  
  # Add a new row to the table given the user's input from the modal dialog
  observeEvent(input$save_add_post, {
    run_query(query = sprintf(
      "INSERT INTO posts
       ( type, equipment_pk, units, user_pk, clinic_pk, note , status, created_by, created_on )
       VALUES
       ( '%s',           %s,    %s,      %s,        %s, '%s' ,   '%s',         %s, NOW()      );",
      input[[paste0("type_add", input$add_post)]],
      input[[paste0("equipment_pk_add", input$add_post)]],
      input[[paste0("units_add", input$add_post)]],
      input[[paste0("user_pk_add", input$add_post)]],
      input[[paste0("clinic_pk_add", input$add_post)]],
      input[[paste0("note_add", input$add_post)]],
      switch(input[[paste0("type_add", input$add_post)]], "Request" = "Unfilled", "Offer" = "Available", ""),
      user()$user_pk[1]
    ))
    db$posts = load_table("posts")
    generate_message(message = "Your changes have been saved!")
  }, ignoreInit = TRUE)
  
  
  #######################################
  # EXCHANGE FORUM : VIEW / MODIFY POST #
  #######################################
  
  # Utility to determine the row clicked ("selected" post) in data table
  post = eventReactive(input$ir_posts_cell_clicked$row, {
    vw_posts()[input$ir_posts_cell_clicked$row,]
  })
  
  # Utility to generate a technical summary for the selected post
  post_edit_history = eventReactive(post(), {
    list(
      fluidRow(
        column(width = 3, "Created by",
               style = "text-align: left; font-weight: bold;"),
        column(width = 4, post()$created_by,
               style = "text-align: left;"),
        column(width = 1, "on",
               style = "text-align: right; font-weight: bold;"),
        column(width = 4, post()$created_on,
               style = "text-align: right;")
      ),
      fluidRow(
        column(width = 3, "Modified by",
               style = "text-align: left; font-weight: bold;"),
        column(width = 4, post()$modified_by,
               style = "text-align: left;"),
        column(width = 1, "on",
               style = "text-align: right; font-weight: bold;"),
        column(width = 4, post()$modified_on,
               style = "text-align: right;")
      ),
      tags$br()
    )
  })
  
  # Update the list of contact persons in the "Modify Post" modal dialog
  observeEvent(input[[paste0("clinic_pk_mod", post()$post_pk)]], {
    updateSelectInput(
      session,
      inputId = paste0("user_pk_mod", post()$post_pk),
      choices = lookup(vw_user_clinics()[
        vw_user_clinics()$clinic_pk %in% input[[paste0("clinic_pk_mod", post()$post_pk)]],
        c("user_pk", "user_desc")
        ]),
      selected = post()$user_pk
    )
  })
  
  # Update the list of available statuses in the "Modify Post" modal dialog
  observeEvent(input[[paste0("type_mod", post()$post_pk)]], {
    updateSelectInput(
      session,
      inputId = paste0("status_mod", post()$post_pk),
      choices = if (input[[paste0("type_mod", post()$post_pk)]] == "Request") {
        c("Unfilled", "Cancelled", "Completed")
      } else if (input[[paste0("type_mod", post()$post_pk)]] == "Offer") {
        c("Available", "Cancelled", "Taken")
      }
    )
  })
  
  # Generate a modal dialog to either view or modify selected post
  observeEvent(post(), {
    app$current_interact = "mod"
    if (nrow(post()) != 0) {
      if (post()$clinic_pk %in% user()$clinic_pk && post()$status %in% c("Unfilled", "Available")) {
        showModal(modalDialog(
          title = paste("Modify", post()$type),
          fluidPage(
            fluidRow(
              column(width = 3,
                     selectInput(inputId = paste0("type_mod", post()$post_pk),
                                 label = "Post Type",
                                 choices = c("Request", "Offer"),
                                 selected = post()$type)),
              column(width = 7,
                     selectInput(inputId = paste0("equipment_pk_mod", post()$post_pk),
                                 label = "Equipment",
                                 choices = lookup(db$equipment[db$equipment$is_active,]),
                                 selected = post()$equipment_pk)),
              column(width = 2,
                     numericInput(inputId = paste0("units_mod", post()$post_pk),
                                  label = "Units",
                                  value = post()$units,
                                  min = 1))
            ),
            fluidRow(
              column(width = 8,
                     selectInput(inputId = paste0("clinic_pk_mod", post()$post_pk),
                                 label = "Clinic Name",
                                 choices = lookup(user()[, c("clinic_pk", "clinic_desc")]),
                                 selected = post()$clinic_pk)),
              column(width = 4,
                     selectInput(inputId = paste0("user_pk_mod", post()$post_pk),
                                 label = "Contact Name",
                                 choices = numeric(0)))
            ),
            fluidRow(
              column(width = 3,
                     tags$strong("Please contact:")),
              column(width = 9,
                     htmlOutput(outputId = "contact_info"))
            ),
            fluidRow(column(width = 12,
                            textAreaInput2(inputId = paste0("note_mod", post()$post_pk),
                                           label = "Note",
                                           value = post()$note,
                                           rows = 2,
                                           placeholder = "No PHI please.")))
          ),
          easyClose = TRUE,
          footer = fluidPage(
            fluidRow(
              column(width = 3,
                     tags$strong("Post Status:"),
                     class = "text-middle"),
              column(width = 5,
                     selectInput(inputId = paste0("status_mod", post()$post_pk),
                                 label = NULL,
                                 choices = if (post()$type == "Request") {
                                   c("Unfilled", "Cancelled", "Completed")
                                 } else if (post()$type == "Offer") {
                                   c("Available", "Cancelled", "Taken")
                                 })),
              column(width = 4,
                     tags$div(actionButton(inputId = "save_mod_post",
                                           label = "Save Changes",
                                           icon = icon("save"))),
                     tags$div(hidden(actionButton(inputId = "confirm_mod_post",
                                                  label = "Confirm Changes",
                                                  icon = icon("check")))))
            ),
            hidden(fluidRow(
              id = "confirm_mod_post_message",
              column(width = 12,
                     paste("Posts marked as \"Cancelled\", \"Completed\", or \"Taken\"",
                           "are closed permanently and cannot be reopened.",
                           "Please click again to confirm you'd like to close this request."),
                     style = "color: red; padding-top: 5px;",
                     class = "text-center")
            )),
            tags$hr(),
            post_edit_history()
          ),
          size = "m"
        ))
      } else {
        showModal(modalDialog(
          title = paste("View", post()$type),
          fluidPage(
            fluidRow(
              column(width = 3,
                     HTML(paste0("<strong>", post()$type, "ing:</strong>"))),
              column(width = 1,
                     paste0(post()$units, "x")),
              column(width = 8,
                     HTML(paste0(post()$equipment_desc, "<br/><br/>")))
            ),
            fluidRow(
              column(width = 3,
                     HTML("<strong>Please contact:</strong>")),
              column(width = 9,
                     htmlOutput(outputId = "contact_info"))
            ),
            fluidRow(
              column(width = 3,
                     HTML("<strong>Status:</strong>")),
              column(width = 9,
                     HTML(paste0(post()$status, "<br/><br/>")))
            ),
            if (post()$note != "") {
              fluidRow(
                column(width = 3,
                       HTML("<strong>Note:</strong>")),
                column(width = 9,
                       HTML(paste0(post()$note, "<br/><br/>"))))
            }
          ),
          easyClose = TRUE,
          footer = fluidPage(post_edit_history())
        ))
      }
    }
  })
  
  # Hide confirmation button and show save button on open modal
  observeEvent(post(), showElement(id = "save_mod_post"))
  
  # In case user is closing post, prompt user for confirmation
  observeEvent(input$save_mod_post, {
    if (input[[paste0("status_mod", post()$post_pk)]] %in% c("Unfilled", "Available")) {
      click(id = "confirm_mod_post")
    } else {
      hideElement(id = "save_mod_post")
      showElement(id = "confirm_mod_post")
      showElement(id = "confirm_mod_post_message")
    }
  })
  
  # Update row in posts table given the user's input from the modal dialog
  observeEvent(input$confirm_mod_post, {
    run_query(query = sprintf(
      "UPDATE posts SET
           type = '%s'
         , equipment_pk = %s
         , units = %s
         , clinic_pk = %s
         , user_pk = %s
         , note = '%s'
         , status = '%s'
         , modified_by = %s
         , modified_on = NOW()
       WHERE post_pk = %s;",
      input[[paste0("type_mod", post()$post_pk)]],
      input[[paste0("equipment_pk_mod", post()$post_pk)]],
      input[[paste0("units_mod", post()$post_pk)]],
      input[[paste0("clinic_pk_mod", post()$post_pk)]],
      input[[paste0("user_pk_mod", post()$post_pk)]],
      gsub("'", "\\\\'", input[[paste0("note_mod", post()$post_pk)]]),
      input[[paste0("status_mod", post()$post_pk)]],
      user()$user_pk,
      post()$post_pk
    ))
    db$posts = load_table("posts")
    generate_message(message = "Your changes have been saved!")
  }, ignoreInit = TRUE)
  
  
  #############################
  # SETTINGS : MAIN INTERFACE #
  #############################
  
  # Create options for possible settings to change
  observeEvent(app$user_pk, {#app$authenticated, {
    app$settings_menu = c(
      "User Profile" = "user_profile",
      "Security and Login" = "security_and_login",
      "Edit Clinics" = "edit_clinics",
      "Edit Clinic Staff" = "edit_staff",
      "Edit Clinic Groups" = "edit_groups"
    )
    if (user()$is_super_admin) {
      app$settings_menu = append(app$settings_menu, c("(S) Create User Accounts" = "create_users"))
      app$settings_menu = append(app$settings_menu, c("(S) Reset User Password" = "reset_password"))
      app$settings_menu = append(app$settings_menu, c("(S) Edit Equipment" = "edit_equipment"))
    }
  }, ignoreInit = TRUE, once = TRUE)
  
  # Create settings menu with the options on the side and interface centered
  output$settings = renderUI({
    fixedPage(sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "settings_menu",
                     label = "Settings",
                     choices = app$settings_menu),
        width = 3
      ),
      mainPanel(uiOutput(outputId = "settings_main_panel"), width = 9)
    ))
  })
  
  # Create menu panel for settings
  output$settings_main_panel = renderUI({
    uiOutput(outputId = input$settings_menu)
  })
  
  
  ###########################
  # SETTINGS : USER PROFILE #
  ###########################
  
  # Allows user to change first name, middle name, last name, and phone number
  output$user_profile = renderUI({
    if (input$settings_menu == "user_profile") {
      fixedPage(
        fixedRow(column(width = 9,
                        tags$h3("User Profile"))),
        tags$br(),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "first_name_label",
                 tags$strong("First Name"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "first_name",
                           label = NULL,
                           value = user()$first_name[1],
                           placeholder = "Required"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 tags$strong("Middle Name"),
                 id = "middle_name_label",
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "middle_name",
                           label = NULL,
                           value = user()$middle_name[1],
                           placeholder = "Optional"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "last_name_label",
                 tags$strong("Last Name"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "last_name",
                           label = NULL,
                           value = user()$last_name[1],
                           placeholder = "Required"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "phone_label",
                 tags$strong("Phone Number"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "phone",
                           label = NULL,
                           value = user()$phone[1],
                           placeholder = "Optional (10 digits)"))
        ),
        tags$br(),
        tags$br(),
        fixedRow(column(width = 2,
                        actionButton(inputId = "save_user_profile",
                                     label = "Save Changes",
                                     icon = icon("save")
                        ),
                        offset = 4))
      )
    }
  })
  
  # Performs checks on edited fields
  observeEvent(input$save_user_profile, {
    errors = list()
    first_name = trimws(isolate(input$first_name))
    middle_name = trimws(isolate(input$middle_name))
    last_name = trimws(isolate(input$last_name))
    phone = gsub("[^[:digit:]]", "", isolate(input$phone))
    if (nchar(first_name) < 1 | nchar(first_name) > 45) {
      addClass(id = "first_name_label", class = "text-red")
      errors = append(errors, "First Name must be between 1 and 45 characters long.")
    } else {
      removeClass(id = "first_name_label", class = "text-red")
    }
    if (nchar(middle_name) > 45) {
      addClass(id = "middle_name_label", class = "text-red")
      errors = append(errors, "Middle Name must be shorter than 45 characters.")
    } else {
      removeClass(id = "middle_name_label", class = "text-red")
    }
    if (nchar(last_name) < 1 | nchar(last_name) > 45) {
      addClass(id = "last_name_label", class = "text-red")
      errors = append(errors, "Last Name must be between 1 and 45 characters long.")
    } else {
      removeClass(id = "last_name_label", class = "text-red")
    }
    if (!nchar(phone) %in% c(0, 10)) {
      addClass(id = "phone_label", class = "text-red")
      errors = append(errors, "Phone number must have exactly 10 digits.")
    } else {
      removeClass(id = "phone_label", class = "text-red")
    }
    if (length(errors) > 0) {
      generate_error(errors)
    } else {
      run_query(query = sprintf(
        "UPDATE users SET
             first_name = '%s'
           , middle_name = CASE WHEN '%s' = '' THEN NULL ELSE '%s' END
           , last_name = '%s'
           , phone = %s
         WHERE user_pk = %s;",
        gsub("'", "\\\\'", first_name),
        gsub("'", "\\\\'", middle_name),
        gsub("'", "\\\\'", middle_name),
        gsub("'", "\\\\'", last_name),
        if (phone == "") {
          "DEFAULT"
        } else {
          paste0("'(", substr(phone, 1, 3), ") ", substr(phone, 4, 6), "-", substr(phone, 7, 10),"'")
        },
        user()$user_pk
      ))
      db$users = load_table("users")
      generate_message(message = "Your changes have been saved!")
    }
  }, ignoreInit = TRUE)
  
  
  #################################
  # SETTINGS : SECURITY AND LOGIN #
  #################################
  
  # Allows user to change email address and/or password
  output$security_and_login = renderUI({
    if (input$settings_menu == "security_and_login") {
      fixedPage(
        fixedRow(column(width = 9,
                        tags$h3("Security and Login"))),
        tags$br(),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "email_mod_label",
                 tags$strong("Email Address"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "email_mod",
                           label = NULL,
                           value = user()$email[1],
                           placeholder = "@your_company.com"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "current_password_label",
                 tags$strong("Current Password"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 passwordInput(inputId = "current_password",
                               label = NULL,
                               placeholder = "Required"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "password_mod_label",
                 tags$strong("New Password"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 passwordInput(inputId = "password_mod",
                               label = NULL,
                               placeholder = "Optional"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "password_mod2_label",
                 tags$strong("Retype New Password"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 passwordInput(inputId = "password_mod2",
                               label = NULL,
                               placeholder = "Optional"))
        ),
        tags$br(),
        tags$br(),
        fixedRow(column(width = 2,
                        actionButton(inputId = "save_security_and_login",
                                     label = "Save Changes",
                                     icon = icon("save")),
                        offset = 4))
      )
    }
  })
  
  # Performs checks on edited fields
  observeEvent(input$save_security_and_login, {
    current_password = isolate(input$current_password)
    if (!password_verify(hash = user()$password[1], password = current_password)) {
      addClass(id = "current_password_label", class = "text-red")
      generate_message(message = "You must enter your current password correctly to make any edits.")
    } else {
      removeClass(id = "current_password_label", class = "text-red")
      errors = list()
      email_mod = trimws(tolower(isolate(input$email_mod)))
      password_mod = isolate(input$password_mod)
      password_mod2 = isolate(input$password_mod2)
      if (email_mod %in% db$users$email[db$users$user_pk != user()$user_pk[1]]) {
        addClass(id = "email_mod_label", class = "text-red")
        errors = append(errors, "This email address is already in use.")
      } else if (!grepl("^[[:alnum:]\\._-]+@your_company\\.com$", email_mod) |
                 nchar(email_mod) > 147) {
        addClass(id = "email_mod_label", class = "text-red")
        errors = append(errors, "Must be a valid company email address.")
      } else {
        removeClass(id = "email_mod_label", class = "text-red")
      }
      if (password_mod != password_mod2) {
        addClass(id = "password_mod_label", class = "text-red")
        addClass(id = "password_mod2_label", class = "text-red")
        errors = append(errors, "New passwords do not match.")
      } else {
        removeClass(id = "password_mod_label", class = "text-red")
        removeClass(id = "password_mod2_label", class = "text-red")
      }
      if (length(errors) > 0) {
        generate_error(errors)
      } else if (nchar(password_mod) == 0) {
        run_query(query = sprintf(
          "UPDATE users SET email = '%s' WHERE user_pk = %s",
          email_mod,
          user()$user_pk[1]
        ))
        db$users = load_table("users")
        generate_message(message = "Your changes have been saved!")
      } else {
        run_query(query = sprintf(
          "UPDATE users SET email = '%s', password = '%s' WHERE user_pk = %s;",
          email_mod,
          password_store(password_mod),
          user()$user_pk[1]
        ))
        db$users = load_table("users")
        generate_message(message = "Your changes have been saved!")
      }
    }
  }, ignoreInit = TRUE)
  
  
  ###########################
  # SETTINGS : EDIT CLINICS #
  ###########################
  
  # Create an interactive data table to show clinic's staff
  output$ir_clinics = renderDataTable({
    req(input$settings_menu == "edit_clinics")
    datatable(
      data = db$clinics[db$clinics$clinic_pk != 0, c("clinic_desc", "address1", "address2", "zip")],
      colnames = c("Clinic Name", "Address Line 1", "Address Line 2", "ZIP"),
      escape = FALSE,
      filter = "top",
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "40%"),
                                       list(targets = 1, width = "25%"),
                                       list(targets = 2, width = "25%"),
                                       list(targets = 3, width = "10%")),
                     dom = "it",
                     order = list(0, "asc"),
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the table of PPE posts submitted
  output$edit_clinics = renderUI({
    if (input$settings_menu == "edit_clinics") {
      fixedPage(
        fixedRow(column(
          width = 9,
          tags$h3("Edit Clinics"),
          helpText(paste(
            "You can only edit clinics' information for which",
            "you are designated a site admin.",
            "Please do not add clinics that already exist below."
          ))
        )),
        tags$br(),
        tags$br(),
        fixedRow(column(width = 2,
                        actionButton(inputId = "add_clinic",
                                     label = "Add Clinic",
                                     icon = icon("hospital-alt")),
                        offset = 7)),
        tags$br(),
        fixedRow(column(width = 9, dataTableOutput(outputId = "ir_clinics")))
      )
    }
  })
  
  # Generate a modal dialog to accept user input regarding new clinic
  observeEvent(input$add_clinic, {
    showModal(modalDialog(
      title = "Add Clinic",
      fluidPage(
        fluidRow(column(width = 12,
                        textInput(inputId = paste0("clinic_desc_add", input$add_clinic),
                                  label = "Clinic Name",
                                  placeholder = "Required"))),
        hidden(fluidRow(
          id = "clinic_desc_add_message",
          column(width = 12, "Clinic Name must be unique, and between 1 and 90 characters."),
          style = "color: red; text-align: center;"
        )),
        fluidRow(column(
          width = 12,
          textInput(inputId = paste0("address1_add", input$add_clinic),
                    label = "Address Line 1",
                    placeholder = "Required")
        )),
        hidden(fluidRow(
          id = "address1_add_message",
          column(width = 12, "Address Line 1 must be between 1 and 45 characters."),
          style = "color: red; text-align: center;"
        )),
        fluidRow(column(width = 12,
                        textInput(inputId = paste0("address2_add", input$add_clinic),
                                  label = "Address Line 2",
                                  placeholder = "Optional"))),
        hidden(fluidRow(
          id = "address2_add_message",
          column(width = 12, "Address Line 2 must be less than 45 characters."),
          style = "color: red; text-align: center;"
        )),
        fluidRow(
          column(width = 6,
                 disabled(textInput(
                   inputId = paste0("city_state_add", input$add_clinic),
                   label = "City, State",
                   value = "San Francisco, CA"
                 ))),
          column(width = 6,
                 selectInput(inputId = paste0("zip_add", input$add_clinic),
                             label = "ZIP",
                             choices = valid_zips))
        )
      ),
      footer = fluidPage(
        fluidRow(column(width = 4,
                        actionButton(inputId = "save_add_clinic",
                                     label = "Add Clinic",
                                     icon = icon("hospital-alt")),
                        offset = 8)),
        tags$br()
      ),
      easyClose = TRUE,
      size = "m"
    ))
  }, ignoreInit = TRUE)
  
  # Perform checks and add new row to clinics table if appropriate
  observeEvent(input$save_add_clinic, {
    errors = FALSE
    clinic_desc = isolate(input[[paste0("clinic_desc_add", input$add_clinic)]])
    address1 = isolate(input[[paste0("address1_add", input$add_clinic)]])
    address2 = isolate(input[[paste0("address2_add", input$add_clinic)]])
    zip = isolate(input[[paste0("zip_add", input$add_clinic)]])
    if (nchar(clinic_desc) < 1 | nchar(clinic_desc) > 90 | clinic_desc %in% db$clinics$clinic_desc) {
      errors = TRUE
      showElement(id = "clinic_desc_add_message")
    } else {
      hideElement(id = "clinic_desc_add_message")
    }
    if (nchar(address1) < 1 | nchar(address1) > 45) {
      errors = TRUE
      showElement(id = "address1_add_message")
    } else {
      hideElement(id = "address1_add_message")
    }
    if (nchar(address2) > 45) {
      errors = TRUE
      showElement(id = "address2_add_message")
    } else {
      hideElement(id = "address2_add_message")
    }
    if (!errors) {
      query = sprintf(
        "INSERT INTO clinics ( clinic_desc, address1, address2,  zip )
         VALUES              (        '%s',     '%s',       %s, '%s' );",
        clinic_desc,
        gsub("'", "\\\\'", address1),
        if (address2 == "") "DEFAULT" else paste0("'", gsub("'", "\\\\'", address2), "'"),
        zip
      )
      print(query)
      run_query(query)
      db$clinics = load_table("clinics")
      run_query(query = sprintf(
        "INSERT INTO user_clinic_interface ( user_pk, clinic_pk, is_site_admin )
         VALUES                            (      %s,        %s,             1 );",
        user()$user_pk[1],
        db$clinics$clinic_pk[db$clinics$clinic_desc == clinic_desc]
      ))
      db$user_clinic_interface = load_table("user_clinic_interface")
      generate_message(message = "Your changes have been saved!")
    }
  }, ignoreInit = TRUE)
  
  # Utility to determine the row clicked ("selected" clinic) in data table
  clinic = eventReactive(input$ir_clinics_cell_clicked$row, {
    db$clinics[db$clinics$clinic_pk != 0,][input$ir_clinics_cell_clicked$row,]
  })
  
  # Generate a modal dialog to modify selected clinic if user is site admin
  observeEvent(clinic(), {
    showModal(modalDialog(
      title = "Modify Clinic",
      fluidPage(
        fluidRow(column(width = 12,
                        textInput(inputId = paste0("clinic_desc_mod", clinic()$clinic_pk),
                                  label = "Clinic Name",
                                  placeholder = "Required",
                                  value = clinic()$clinic_desc))),
        hidden(fluidRow(id = "clinic_desc_mod_message",
                        column(width = 12, "Clinic Name must be between 1 and 90 characters."),
                        style = "color: red; text-align: center;")),
        fluidRow(column(width = 12,
                        textInput(inputId = paste0("address1_mod", clinic()$clinic_pk),
                                  label = "Address Line 1",
                                  placeholder = "Required",
                                  value = clinic()$address1))),
        hidden(fluidRow(id = "address1_mod_message",
                        column(width = 12, "Address Line 1 must be between 1 and 45 characters."),
                        style = "color: red; text-align: center;"
        )),
        fluidRow(column(width = 12,
                        textInput(inputId = paste0("address2_mod", clinic()$clinic_pk),
                                  label = "Address Line 2",
                                  placeholder = "Optional",
                                  value = clinic()$address2))),
        hidden(fluidRow(id = "address2_mod_message",
                        column(width = 12, "Address Line 2 must be less than 45 characters."),
                        style = "color: red; text-align: center;")),
        fluidRow(
          column(width = 6,
                 disabled(textInput(inputId = paste0("city_state_mod", clinic()$clinic_pk),
                                    label = "City, State",
                                    value = "San Francisco, CA"))),
          column(width = 6,
                 selectInput(inputId = paste0("zip_mod", clinic()$clinic_pk),
                             label = "ZIP",
                             choices = valid_zips,
                             selected = clinic()$zip))
        ),
        hidden(fluidRow(id = "zip_mod_message",
                        column(width = 12, "Please select a valid San Francisco zip code."),
                        style = "color: red; text-align: center;")),
        fluidRow(column(width = 12,
                        disabled(textAreaInput2(inputId = paste0("site_admins_mod", clinic()$clinic_pk),
                                                label = "Site Admin(s)",
                                                placeholder = "(None)",
                                                value = paste(vw_user_clinics()$user_tag[
                                                  vw_user_clinics()$clinic_pk %in% clinic()$clinic_pk
                                                  ], collapse = "\n"),
                                                rows = 4))))
      ),
      footer = fluidPage(
        fluidRow(column(width = 5,
                        actionButton(inputId = "save_mod_clinic",
                                     label = "Save Changes",
                                     icon = icon("save")),
                        offset = 7)),
        tags$br()
      ),
      easyClose = TRUE,
      size = "m"
    ))
  })
  
  # Disable inputs if user is not a site admin
  observeEvent(clinic(), {
    if (clinic()$clinic_pk %in% user()$clinic_pk[user()$is_site_admin] || user()$is_super_admin[1]) {
      enable(id = paste0("clinic_desc_mod", clinic()$clinic_pk))
      enable(id = paste0("address1_mod", clinic()$clinic_pk))
      enable(id = paste0("address2_mod", clinic()$clinic_pk))
      enable(id = paste0("zip_mod", clinic()$clinic_pk))
      enable(id = paste0("save_mod_clinic"))
    } else {
      disable(id = paste0("clinic_desc_mod", clinic()$clinic_pk))
      disable(id = paste0("address1_mod", clinic()$clinic_pk))
      disable(id = paste0("address2_mod", clinic()$clinic_pk))
      disable(id = paste0("zip_mod", clinic()$clinic_pk))
      disable(id = paste0("save_mod_clinic"))
    }
  })
  
  # Perform checks and edit row in clinics table if appropriate
  observeEvent(input$save_mod_clinic, {
    errors = FALSE
    clinic_desc = isolate(input[[paste0("clinic_desc_mod", clinic()$clinic_pk)]])
    address1 = isolate(input[[paste0("address1_mod", clinic()$clinic_pk)]])
    address2 = isolate(input[[paste0("address2_mod", clinic()$clinic_pk)]])
    zip = isolate(input[[paste0("zip_mod", clinic()$clinic_pk)]])
    if (nchar(clinic_desc) < 1 | nchar(clinic_desc) > 90) {
      errors = TRUE
      showElement(id = "clinic_desc_mod_message")
    } else {
      hideElement(id = "clinic_desc_mod_message")
    }
    if (nchar(address1) < 1 | nchar(address1) > 45) {
      errors = TRUE
      showElement(id = "address1_mod_message")
    } else {
      hideElement(id = "address1_mod_message")
    }
    if (nchar(address2) > 45) {
      errors = TRUE
      showElement(id = "address2_mod_message")
    } else {
      hideElement(id = "address2_mod_message")
    }
    if (nchar(zip) != 5) {
      errors = TRUE
      showElement(id = "zip_mod_message")
    } else {
      hideElement(id = "zip_mod_message")
    }
    if (!errors) {
      run_query(query = sprintf(
        "UPDATE clinics SET
             clinic_desc = '%s'
           , address1 = '%s'
           , address2 = %s
           , zip = '%s'
         WHERE clinic_pk = %s;",
        clinic_desc,
        gsub("'", "\\\\'", address1),
        if (address2 == "") "DEFAULT" else paste0("'", gsub("'", "\\\\'", address2), "'"),
        zip,
        clinic()$clinic_pk
      ))
      db$clinics = load_table("clinics")
      generate_message(message = "Your changes have been saved!")
    }
  }, ignoreInit = TRUE)
  
  
  ################################
  # SETTINGS : EDIT CLINIC STAFF #
  ################################
  
  # Create a view of clinic's current staff
  vw_staff = reactive({
    req(input$edit_staff_clinic_pk != 0 && input$settings_menu == "edit_staff")
    vw_user_clinics()[vw_user_clinics()$clinic_pk %in% input$edit_staff_clinic_pk,]
  })
  
  # Create an interactive data table to show clinic's staff
  output$ir_staff = renderDataTable({
    req(input$settings_menu == "edit_staff")
    datatable(
      data = vw_staff()[, c("user_desc", "email", "phone", "is_site_admin")],
      colnames = c("Name", "Email", "Phone", "Site Admin"),
      escape = FALSE,
      filter = "top",
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "30%"),
                                       list(targets = 1, width = "40%"),
                                       list(targets = 2, width = "15%"),
                                       list(targets = 3, width = "15%")),
                     dom = "it",
                     order = list(0, "asc"),
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the table of a clinics' staff
  output$edit_staff = renderUI({
    if (input$settings_menu == "edit_staff") {
      fixedPage(
        fixedRow(column(
          width = 9,
          tags$h3("Edit Clinic Staff"),
          helpText("You can only edit staff at clinics for which you are designated a site admin.")
        )),
        tags$br(),
        tags$br(),
        fixedRow(
          column(width = 5,
                 selectInput(inputId = "edit_staff_clinic_pk",
                             label = "Select a Clinic",
                             choices = lookup(user()[, c("clinic_pk", "clinic_desc")]))),
          column(width = 2,
                 actionButton(inputId = "add_staff",
                              label = "Add Staff",
                              icon = icon("user-plus"),
                              style = "margin-top: 25px;"),
                 offset = 2)
        ),
        tags$br(),
        fixedRow(column(width = 9, dataTableOutput(outputId = "ir_staff")))
      )
    }
  })
  
  # Generate a modal dialog to accept user input regarding new staff
  observeEvent(input$add_staff, {
    if (input$edit_staff_clinic_pk %in% user()$clinic_pk[user()$is_site_admin] || user()$is_super_admin[1]) {
      showModal(modalDialog(
        title = "Add Staff",
        fluidPage(fluidRow(
          column(width = 9,
                 selectInput(inputId = "add_staff_user_pk",
                             label = "Staff Name(s)",
                             choices = lookup(db$users[!db$users$user_pk %in% vw_staff()$user_pk,],
                                              mask = "tag"),
                             multiple = TRUE)),
          column(width = 3,
                 tags$br(),
                 checkboxInput(inputId = "add_staff_is_site_admin",
                               label = "Site Admin"))
        )),
        footer = fluidPage(
          fluidRow(column(width = 4,
                          actionButton(inputId = "save_add_staff",
                                       label = "Add Staff",
                                       icon = icon("user-plus")),
                          offset = 8)),
          tags$br()
        ),
        easyClose = TRUE,
        size = "m"
      ))
    } else {
      generate_message(message = paste(
        "You can only edit staff at clinics for which you",
        "are designated a site admin. Contact an",
        "existing admin to be granted this access level."
      ))
    }
  }, ignoreInit = TRUE)
  
  # Add a new row to the table given the user's input from the modal dialog
  observeEvent(input$save_add_staff, {
    run_query(query = paste0(
      "INSERT INTO user_clinic_interface
       ( user_pk, clinic_pk, is_site_admin )
       VALUES",
      paste0("(", input$add_staff_user_pk, ",",
             input$edit_staff_clinic_pk,
             ",", as.numeric(input$add_staff_is_site_admin), ")",
             collapse = ","),
      ";"
    ))
    db$user_clinic_interface = load_table("user_clinic_interface")
    generate_message(message = "Your changes have been saved!")
  }, ignoreInit = TRUE)
  
  # Utility to determine the row clicked ("selected" staff) in data table
  staff = eventReactive(input$ir_staff_cell_clicked$row, {
    vw_staff()[input$ir_staff_cell_clicked$row,]
  })
  
  # Generate a modal dialog to either view or modify selected staff
  observeEvent(staff(), {
    if (user()$is_site_admin[user()$clinic_pk == input$edit_staff_clinic_pk] || user()$is_super_admin[1]) {
      showModal(modalDialog(
        title = "Modify Staff",
        fluidPage(fluidRow(
          column(width = 8,
                 disabled(selectInput(inputId = "mod_staff_user_pk",
                                      label = "Staff Name",
                                      choices = staff()$user_tag))),
          column(width = 4,
                 tags$br(),
                 checkboxInput(inputId = "mod_staff_is_site_admin",
                               label = "Site Admin",
                               value = staff()$is_site_admin))
        )),
        footer = fluidPage(
          fluidRow(column(width = 4,
                          actionButton(inputId = "save_del_staff",
                                       label = "Remove Staff",
                                       icon = icon("user-minus"))),
                   column(width = 4,
                          actionButton(inputId = "save_mod_staff",
                                       label = "Save Changes",
                                       icon = icon("save")),
                          offset = 4)),
          tags$br()
        ),
        easyClose = TRUE,
        size = "m"
      ))
    }
  })
  
  # Update row in user_clinic_interface table given the user's input from the modal dialog
  observeEvent(input$save_mod_staff, {
    run_query(query = sprintf(
      "UPDATE user_clinic_interface SET is_site_admin = %s WHERE user_clinic_pk = %s;",
      as.numeric(input$mod_staff_is_site_admin),
      staff()$user_clinic_pk
    ))
    db$user_clinic_interface = load_table("user_clinic_interface")
    generate_message(message = "Your changes have been saved!")
  }, ignoreInit = TRUE)
  
  # Delete row in user_clinic_interface table given the user's input from the modal dialog
  observeEvent(input$save_del_staff, {
    run_query(query = sprintf(
      "DELETE FROM user_clinic_interface WHERE user_clinic_pk = %s;",
      staff()$user_clinic_pk
    ))
    db$user_clinic_interface = load_table("user_clinic_interface")
    generate_message(message = "Your changes have been saved!")
  })
  
  
  #################################
  # SETTINGS : EDIT CLINIC GROUPS #
  #################################
  
  # Create a view of the clinics associated with a group
  vw_group = reactive({
    req(input$settings_menu == "edit_groups")
    x = merge(x = db$groups[db$groups$group_pk == input$edit_groups_group_pk,],
              y = db$group_clinic_interface,
              all.x = TRUE)
    merge(x, db$clinics, all.x = TRUE)
  })
  
  # Create an interactive data table to show clinic's staff
  output$ir_group = renderDataTable({
    req(input$settings_menu == "edit_groups")
    datatable(
      data = vw_group()[, c("clinic_desc", "address1", "address2", "zip")],
      colnames = c("Clinic Name", "Address Line 1", "Address Line 2", "ZIP"),
      escape = FALSE,
      filter = "top",
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "40%"),
                                       list(targets = 1, width = "25%"),
                                       list(targets = 2, width = "25%"),
                                       list(targets = 3, width = "10%")),
                     dom = "it",
                     order = list(0, "asc"),
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the table of a clinics' staff
  output$edit_groups = renderUI({
    if (input$settings_menu == "edit_groups") {
      fixedPage(
        fixedRow(column(
          width = 9,
          tags$h3("Edit Clinic Groups"),
          helpText("You can only edit clinic groups for which you are designated a group admin.")
        )),
        tags$br(),
        tags$br(),
        fixedRow(
          column(width = 5,
                 selectInput(inputId = "edit_groups_group_pk",
                             label = "Select a Clinic Group",
                             choices = lookup(db$groups))),
          column(width = 2,
                 actionButton(inputId = "edit_group_admins",
                              label = "Edit Group Admins",
                              icon = icon("user-shield"),
                              style = "margin-top: 25px;")),
          column(width = 2,
                 actionButton(inputId = "add_group_member",
                              label = "Add Group Member",
                              icon = icon("plus"),
                              style = "margin-top: 25px;"))
        ),
        tags$br(),
        fixedRow(column(width = 9, dataTableOutput(outputId = "ir_group")))
      )
    }
  })
  
  # Generate a modal dialog to accept user input regarding editing a group's admins
  observeEvent(input$edit_group_admins, {
    showModal(modalDialog(
      title = "Edit Group Admins",
      fluidPage(
        fluidRow(column(width = 12,
                        selectInput(inputId = paste0("mod_group_admins", input$edit_groups_group_pk),
                                    label = "Group Admins (up to 5):",
                                    choices = lookup(db$users, mask = "tag"),
                                    selected = strsplit(vw_group()$group_admins[1], ":")[[1]],
                                    multiple = TRUE))),
        hidden(fluidRow(id = "mod_group_admins_message",
                        column(width = 12, "Can only have up to 5 group admins."),
                        style = "color: red; text-align: center;")),
      ),
      footer = fluidPage(
        fluidRow(column(width = 4,
                        actionButton(inputId = "save_mod_group_admins",
                                     label = "Save Changes",
                                     icon = icon("save")),
                        offset = 8)),
        tags$br()
      ),
      easyClose = TRUE,
      size = "m"
    ))
    if (!grepl(app$user_pk, vw_group()$group_admins[1]) && !user()$is_super_admin[1]) {
      disable(id = paste0("mod_group_admins", input$edit_groups_group_pk))
      disable(id = paste0("save_mod_group_admins"))
    }
  })
  
  # Update row in groups table given the user's input from the modal dialog
  observeEvent(input$save_mod_group_admins, {
    mod_group_admins = isolate(input[[paste0("mod_group_admins", input$edit_groups_group_pk)]])
    if (length(mod_group_admins) > 5) {
      showElement(id = "mod_group_admins_message")
    } else {
      run_query(query = sprintf(
        "UPDATE groups SET group_admins = '%s' WHERE group_pk = %s;",
        paste(mod_group_admins, collapse = ":"),
        input$edit_groups_group_pk
      ))
      db$groups = load_table("groups")
      generate_message(message = "Your changes have been saved!")
    }
  }, ignoreInit = TRUE)
  
  # Generate a modal dialog to accept user input regarding adding a clinic to a group
  observeEvent(input$add_group_member, {
    if (grepl(app$user_pk, vw_group()$group_admins[1]) || user()$is_super_admin[1]) {
      showModal(modalDialog(
        title = "Add Group Member",
        fluidPage(fluidRow(
          column(width = 12,
                 selectInput(inputId = "add_group_member_pk",
                             label = "Clinic Name",
                             choices = lookup(db$clinics[
                               !db$clinics$clinic_pk %in% vw_group()$clinic_pk &
                                 db$clinics$clinic_pk != 0,
                               ]),
                             multiple = TRUE))
        )),
        footer = fluidPage(
          fluidRow(column(width = 4,
                          actionButton(inputId = "save_add_group_member",
                                       label = "Add Group Member",
                                       icon = icon("plus")),
                          offset = 8)),
          tags$br()
        ),
        easyClose = TRUE,
        size = "m"
      ))
    } else {
      generate_message(message = paste(
        "You can only edit members of group for which you",
        "are designated a group admin. Contact an",
        "existing admin to be granted this access level."
      ))
    }
  }, ignoreInit = TRUE)
  
  # Add a new row to the table given the user's input from the modal dialog
  observeEvent(input$save_add_group_member, {
    run_query(query = paste0(
      "INSERT INTO group_clinic_interface
       ( group_pk, clinic_pk )
       VALUES",
      paste0("(", input$edit_groups_group_pk, ",", input$add_group_member_pk, ")", collapse = ","),
      ";"
    ))
    db$group_clinic_interface = load_table("group_clinic_interface")
    generate_message(message = "Your changes have been saved!")
  }, ignoreInit = TRUE)
  
  # Utility to determine the row clicked ("selected" group member) in data table
  group_member = eventReactive(input$ir_group_cell_clicked$row, {
    vw_group()[input$ir_group_cell_clicked$row,]
  })
  
  # Generate a modal dialog to either view or modify selected staff
  observeEvent(group_member(), {
    showModal(modalDialog(
      title = "Modify Group Member",
      fluidPage(fluidRow(column(width = 12,
                                disabled(selectInput(inputId = "mod_group_member_clinic_pk",
                                                     label = "Clinic Name",
                                                     choices = group_member()$clinic_desc))))),
      footer = fluidPage(
        fluidRow(column(width = 5,
                        actionButton(inputId = "save_del_group_member",
                                     label = "Remove Group Member",
                                     icon = icon("minus")),
                        offset = 7)),
        tags$br()
      ),
      easyClose = TRUE,
      size = "m"
    ))
    if (!grepl(app$user_pk, group_member()$group_admins[1]) && !user()$is_super_admin[1]) {
      disable(id = "save_del_group_member")
    }
  })
  
  # Delete row in group_clinic_interface table given the user's input from the modal dialog
  observeEvent(input$save_del_group_member, {
    run_query(query = sprintf(
      "DELETE FROM group_clinic_interface WHERE group_clinic_pk = %s;",
      group_member()$group_clinic_pk
    ))
    db$group_clinic_interface = load_table("group_clinic_interface")
    generate_message(message = "Your changes have been saved!")
  })
  
  
  #######################################
  # SETTINGS : (S) CREATE USER ACCOUNTS #
  #######################################
  
  # Generate a page on which to accept user input regarding creating new user accounts 
  output$create_users = renderUI({
    if (input$settings_menu == "create_users") {
      fixedPage(
        fixedRow(column(
          width = 9,
          tags$h3("Create User Accounts"),
          helpText("Please follow the instructions below to create accounts for new users.")
        )),
        tags$br(),
        tags$br(),
        tags$h4("Step 1: Download the *.csv template."),
        fixedRow(column(
          width = 3,
          downloadButton(outputId = "create_users_1_download",
                         label = "Download Template",
                         icon = icon("download"),
                         width = "30%")
        )),
        tags$br(),
        tags$br(),
        tags$h4("Step 2: Fill out the template as a table."),
        paste(
          "Open the template in MS Excel and",
          "fill out one row with the following information",
          "for each user account to create:"
        ),
        tags$ul(
          tags$li(tags$strong("email"),
                  "(required) email address not already in use, up to 147 characters"),
          tags$li(tags$strong("first_name"), "(required) up to 45 characters"),
          tags$li(tags$strong("middle_name"), "(optional) up to 45 characters"),
          tags$li(tags$strong("last_name"), "(required) up to 45 characters"),
          tags$li(tags$strong("phone"), "(optional) exactly 10 digits"),
          tags$li(tags$strong("password"), "(optional) default if left blank is", tags$u("Welcome!"))
        ),
        tags$br(),
        tags$h4("Step 3: Re-upload the template with your edits here."),
        fixedRow(column(
          width = 3,
          fileInput(inputId = "create_users_3_upload",
                    label = NULL,
                    accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        )),
        tags$br(),
        tags$h4("Step 4: Check for any errors or invalid input."),
        fixedRow(column(
          width = 9,
          uiOutput(outputId = "create_users_4_check")
        )),
        tags$br(),
        tags$br(),
        tags$h4("Step 5: Push changes to database."),
        fixedRow(column(
          width = 3,
          disabled(actionButton(inputId = "create_users_5_push",
                                label = "Create User Accounts",
                                icon = icon("user-plus")))
        )),
        tags$br(),
        tags$br()
      )
    }
  })
  
  # Export a blank template for the admin to fill in with user information
  output$create_users_1_download = downloadHandler(
    filename = function(con) paste0("create_user_accounts_template.csv"),
    content = function(con) {
      write.csv(x = data.frame("email" = character(0),
                               "first_name" = character(0),
                               "middle_name" = character(0),
                               "last_name" = character(0),
                               "phone" = character(0),
                               "password" = character(0)),
                file = con,
                row.names = FALSE)
    }
  )
  
  # Read in the file filled out by the super-admin
  create_users_3_upload = eventReactive(input$create_users_3_upload, {
    x = read.csv(input$create_users_3_upload$datapath,
                 colClasses = "character",
                 strip.white = TRUE)
    tryCatch(expr = {
      if (nrow(x) == 0) return("File input has no data.")
      x$email = tolower(x$email)
      x$first_name = gsub("'", "\\\\'", x$first_name)
      x$middle_name = gsub("'", "\\\\'", x$middle_name)
      x$last_name = gsub("'", "\\\\'", x$last_name)
      x$phone = gsub("[^[:digit:]]", "", x$phone)
      x$password = sapply(x$password, function(y) if (y != "") password_store(y) else "")
      x[, c("email", "first_name", "middle_name", "last_name", "phone", "password")]
    }, error = function(e) return("Invalid file input."))
  })
  
  # Check the uploaded file for obvious errors
  create_users_4_check = eventReactive(create_users_3_upload(), {
    x = create_users_3_upload()
    if (class(x) != "data.frame") return(x)
    x = data.frame(
      "email_format" = ifelse(
        test = !grepl("^[[:alnum:]\\._-]+@[[:alnum:]\\._-]+\\.(com|edu|gov|org)$", x$email),
        yes = "Unrecognized email address format.", no = ""
      ),
      "email_len" = ifelse(
        test = nchar(x$email) < 1 | nchar(x$email) > 147,
        yes = "Email address should be between 1 and 147 characters.", no = ""
      ),
      "email_used" = ifelse(
        test = sapply(x$email, function(y) sum(y == c(db$users$email, x$email)) > 1),
        yes = "Duplicate email address found.", no = ""
      ),
      "first_name_len" = ifelse(
        test = nchar(x$first_name) < 1 | nchar(x$first_name) > 45,
        yes = "First name should be between 1 and 45 characters.", no = ""
      ),
      "middle_name_len" = ifelse(
        test = nchar(x$middle_name) > 45,
        yes = "Middle name should be less than 45 characters.", no = ""
      ),
      "last_name_len" = ifelse(
        test = nchar(x$last_name) < 1 | nchar(x$last_name) > 45,
        yes = "Last name should be between 1 and 45 characters.", no = ""
      ),
      "phone_len" = ifelse(
        test = !nchar(x$phone) %in% c(0, 10),
        yes = "Phone number must be exactly 10 digits.", no = ""
      )
    )
    x = trimws(apply(X = x, MARGIN = 1, FUN = paste0, collapse = " "))
    x = sprintf("Row %s: %s", 1:length(x), x)[x != ""]
    if (length(x) > 0) x else "File upload passes all tests."
  })
  
  # Display a message for the super-admin to determine whether any changes need to be made
  output$create_users_4_check = renderUI({
    if (input$settings_menu == "create_users") {
      fixedPage(fixedRow(column(
        width = 9,
        if (is.null(input$create_users_3_upload)) {
          "No file uploaded."
        } else {
          HTML(paste(create_users_4_check(), collapse = "<br />"))
        }
      )))
    }
  })
  
  # Enable the action button to create user accounts once file has passed all tests
  observeEvent(create_users_4_check(), {
    if (create_users_4_check() == "File upload passes all tests.") {
      enable(id = "create_users_5_push")
    } else {
      disable(id = "create_users_5_push")
    }
  })
  
  # Add rows in users table given the super-admin's input
  observeEvent(input$create_users_5_push, {
    x = apply(create_users_3_upload(), MARGIN = 1, function(x) paste0("'", x, "'", collapse = ","))
    x = paste0("(", x, ")", collapse = ",")
    x = gsub("''", "DEFAULT", x)
    run_query(query = paste0(
      "INSERT INTO users ( email, first_name, middle_name, last_name, phone, password )",
      "VALUES ", x, ";"
    ))
    disable(id = "create_users_5_push")
    db$users = load_table("users")
    generate_message(message = "Your changes have been saved!")
  })
  
  
  ######################################
  # SETTINGS : (S) RESER USER PASSWORD #
  ######################################
  
  # Generate a page on which to accept user input regarding resetting a user's password
  output$reset_password = renderUI({
    if (input$settings_menu == "reset_password") {
      fixedPage(
        fixedRow(column(width = 9,
                        tags$h3("Reset User Password"))),
        tags$br(),
        fixedRow(
          column(width = 2,
                 tags$strong("User"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 selectInput(inputId = "reset_password_user_pk",
                             label = NULL,
                             choices = lookup(db$users[!db$users$user_pk %in% c(10000, app$user_pk),],
                                              mask = "tag")))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 tags$strong("Password to Assign"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 textInput(inputId = "reset_password_user_password",
                           label = NULL,
                           placeholder = "Optional (default is Welcome!)"))
        ),
        tags$br(),
        fixedRow(
          column(width = 2,
                 id = "reset_password_admin_password_label",
                 tags$strong("Admin Password"),
                 class = "text-middle",
                 style = "text-align: right;"),
          column(width = 4,
                 passwordInput(inputId = "reset_password_admin_password",
                               label = NULL,
                               placeholder = "Required"))
        ),
        tags$br(),
        tags$br(),
        fixedRow(column(width = 2,
                        actionButton(inputId = "save_reset_password",
                                     label = "Reset Password",
                                     icon = icon("unlock-alt")),
                        offset = 4))
      )
    }
  })
  
  # Update row in users table given the super-admin's input
  observeEvent(input$save_reset_password, {
    user_pk = isolate(input$reset_password_user_pk)
    admin_password = isolate(input$reset_password_admin_password)
    if (password_verify(hash = user()$password[1], password = admin_password)) {
      password = isolate(input$reset_password_user_password)
      run_query(query = sprintf(
        "UPDATE users SET password = %s WHERE user_pk = %s;",
        if (password == "") "DEFAULT" else paste0("'", password_store(password), "'"),
        user_pk
      ))
      reset(id = "reset_password_admin_password")
      removeClass(id = "reset_password_admin_password_label", class = "text-red")
      db$users = load_table("users")
      generate_message(message = "User password has been reset!")
    } else {
      addClass(id = "reset_password_admin_password_label", class = "text-red")
      generate_message(message = "Please enter your password.")
    }
  })
  
  
  #################################
  # SETTINGS : (S) EDIT EQUIPMENT #
  #################################
  
  # Create an interactive data table to show clinic's staff
  output$ir_equipment = renderDataTable({
    req(input$settings_menu == "edit_equipment")
    datatable(
      data = db$equipment[, c("equipment_desc", "is_active")],
      colnames = c("Equipment Name", "Is Active"),
      escape = FALSE,
      filter = "top",
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "80%"),
                                       list(targets = 1, width = "20%")),
                     dom = "it",
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the table of a clinics' staff
  output$edit_equipment = renderUI({
    if (input$settings_menu == "edit_equipment") {
      fixedPage(
        fixedRow(column(
          width = 9,
          tags$h3("Edit Equipment"),
          helpText("Equipment maintains unique IDs, but you may change their name and visibility.")
        )),
        tags$br(),
        tags$br(),
        fixedRow(column(width = 2,
                        actionButton(inputId = "add_equipment",
                                     label = "Create Equipment",
                                     icon = icon("cart-plus"),
                                     style = "margin-top: 25px;"),
                        offset = 7)),
        tags$br(),
        fixedRow(column(width = 9, dataTableOutput(outputId = "ir_equipment")))
      )
    }
  })
  
  # Generate a modal dialog to accept user input regarding new post
  observeEvent(input$add_equipment, {
    showModal(modalDialog(
      title = "Create Equipment",
      fluidPage(fluidRow(
        column(width = 9,
               textInput(inputId = paste0("add_equipment_equipment_desc", input$add_equipment),
                         label = "Equipment Name",
                         placeholder = "Required")),
        column(width = 3,
               id = "add_equipment_is_active",
               tags$br(),
               checkboxInput(inputId = paste0("add_equipment_is_active", input$add_equipment),
                             label = "Is Active",
                             value = TRUE))
      )),
      easyClose = TRUE,
      footer = fluidPage(fluidRow(
        column(width = 8,
               hidden(tags$div(id = "add_equipment_blank_message",
                               "Equipment name cannot be blank.",
                               style = "color: red; padding-top: 5px;"))),
        column(width = 4,
               actionButton(inputId = "save_add_equipment",
                            label = "Create Equipment",
                            icon = icon("cart-plus")))
      )),
      size = "m"
    ))
  })
  
  # Add a new row to the table given the user's input from the modal dialog
  observeEvent(input$save_add_equipment, {
    equipment_desc = isolate(input[[paste0("add_equipment_equipment_desc", input$add_equipment)]])
    if (equipment_desc != "") {
      run_query(query = sprintf(
        "INSERT INTO equipment ( equipment_desc, is_active ) VALUES ( '%s', %s );",
        equipment_desc,
        as.numeric(input[[paste0("add_equipment_is_active", input$add_equipment)]])
      ))
      hideElement(id = "add_equipment_blank_message")
      db$equipment = load_table("equipment")
      generate_message(message = "Your changes have been saved!")
    } else {
      showElement(id = "add_equipment_blank_message")
    }
  })
  
  # Utility to determine the row clicked ("selected" equipment) in data table
  equipment = eventReactive(input$ir_equipment_cell_clicked$row, {
    db$equipment[input$ir_equipment_cell_clicked$row,]
  })
  
  # Generate a modal dialog to modify selected equipment
  observeEvent(equipment(), {
    showModal(modalDialog(
      title = "Modify Equipment",
      fluidPage(fluidRow(
        column(width = 9,
               textInput(inputId = paste0("mod_equipment_equipment_desc", equipment()$equipment_pk),
                         label = "Equipment Name",
                         value = equipment()$equipment_desc,
                         placeholder = "Required")),
        column(width = 3,
               tags$br(),
               checkboxInput(inputId = paste0("mod_equipment_is_active", equipment()$equipment_pk),
                             label = "Is Active",
                             value = equipment()$is_active))
      )),
      easyClose = TRUE,
      footer = fluidPage(fluidRow(
        column(width = 8,
               hidden(tags$div(id = "mod_equipment_blank_message",
                               "Equipment name cannot be blank.",
                               style = "color: red; padding-top: 5px;"))),
        column(width = 4,
               actionButton(inputId = "save_mod_equipment",
                            label = "Save Changes",
                            icon = icon("save")))
      )),
      size = "m"
    ))
  })
  
  # Update row in equipment table given the user's input from the modal dialog
  observeEvent(input$save_mod_equipment, {
    equipment_desc = isolate(input[[paste0("mod_equipment_equipment_desc", equipment()$equipment_pk)]])
    if (equipment_desc != "") {
      run_query(query = sprintf(
        "UPDATE equipment SET equipment_desc = '%s', is_active = %s WHERE equipment_pk = %s;",
        equipment_desc,
        as.numeric(input[[paste0("mod_equipment_is_active", equipment()$equipment_pk)]]),
        equipment()$equipment_pk
      ))
      hideElement(id = "mod_equipment_blank_message")
      db$equipment = load_table("equipment")
      generate_message(message = "Your changes have been saved!")
    } else {
      showElement(id = "mod_equipment_blank_message")
    }
  })
  
  
  ##################################
  # STATUS REPORT : MAIN INTERFACE #
  ##################################
  
  # Create a view to summarize all active posts
  vw_status_report = eventReactive(input$status_report, {
    db$posts = load_table("posts")
    x = db$posts[db$posts$status %in% c("Unfilled", "Available"),]
    if (input$status_report_group_pk != 0) {
      x = x[x$clinic_pk %in% db$clinics$clinic_pk[grep(input$status_report_group_pk, db$clinics$group_pk)],]
    }
    if (nrow(x) == 0) {
      x = data.frame(equipment_desc = character(0),
                     type = character(0),
                     x = numeric(0))
    } else {
      x = aggregate(x = x$units,
                    by = list(equipment_desc = x$equipment_desc, type = x$type),
                    FUN = sum,
                    na.rm = TRUE)
    }
    x = merge(x = db$equipment,
              y = cast(x, equipment_desc ~ type, value = "x"),
              all.x = TRUE)
    for (column in c("Offer", "Request")) {
      if (!column %in% colnames(x)) x[, column] = 0 else x[is.na(x[, column]), column] = 0
    }
    x$Request = -x$Request
    x = x[, c("equipment_desc", "Offer", "Request")]
    colnames(x) = c("Equipment", "Units Offered", "Units Requested")
    x[, "Net Units"] = x[, "Units Offered"] + x[, "Units Requested"]
    x
  })
  
  # Create an interactive report
  output$ir_status_report = renderDataTable({
    req(app$tab == "status_report")
    datatable(
      data = vw_status_report(),
      escape = FALSE,
      options = list(autoWidth = TRUE,
                     columnDefs = list(list(targets = 0, width = "40%"),
                                       list(targets = 1, width = "20%"),
                                       list(targets = 2, width = "20%"),
                                       list(targets = 3, width = "20%")),
                     dom = "t",
                     pageLength = -1),
      rownames = FALSE,
      selection = "none",
      style = "bootstrap"
    )
  })
  
  # Generate the page on which to show the status report
  output$status_report = renderUI({
    fixedPage(sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "status_report_group_pk",
                    label = "Aggregate Group",
                    choices = c("(All)" = 0, lookup(db$groups))),
        downloadButton(outputId = "status_report_download",
                       label = "Download",
                       icon = icon("download")),
        width = 3
      ),
      mainPanel(fixedPage(fixedRow(column(
        width = 9,
        tags$h3("PPE Current Status Report"),
        tags$br(),
        dataTableOutput(outputId = "ir_status_report")
      ))))
    ))
  })
  
  # Provide a utility for user to download the status report
  output$status_report_download = downloadHandler(
    filename = function(con) {
      current_time = format(Sys.time(), "%FT%H%M%S%Z")
      current_time = gsub("PDT$", "-0700", current_time)
      current_time = gsub("PST$", "-0800", current_time)
      paste0("PPE Status Report (", current_time, ").csv")
    },
    content = function(con) {
      write.csv(x = vw_status_report(), con, row.names = FALSE)
    }
  )
  
}
