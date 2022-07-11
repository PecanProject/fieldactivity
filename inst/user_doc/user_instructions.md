---
title: "Instructions to use fieldactivity"
output:
  html_document:
    toc: yes
pagetitle: fieldactivity
---


### Login
In order to use the application, you should have a site account registered. If you don’t have an account yet, see the contact section for requesting one, otherwise you should be able to fill in the provided sitename and password to login.


![Login page](loginpage.png)




### Layout
The UI of the application is simple, but we will provide an overview of what functionalities and views the user can utilise.

1. Change the language between English and Finnish.
2. Check the sitename you have logged in with. Next to this field, by pressing plus sign (+), it gives an option to logout.

![Overview of layout of the application](Layout.png)


3. Change the shown subset of the events based on the type of events, block and year.

As a default, all of the events from all of the (available) blocks and years are visible for the user. With the first option menu, the user is able to select which type of events she wants to keep visible on the summary table. With the second and third option menus, the user can pick the block and the year respectively to be displayed. If there are multiple events displayed, on the bottom of the table there is a navigation tool to switch the shown page.

![Summary table of the events](Eventtable.png)


### Filling in events
Options for filling in the event will open, when pressing the *Add event* button. Start filling in the management event information by first choosing the block wherein the event has occurred, the name of this activity/event and a date for when it was conducted. One may also provide a short description that will be visible in the event table and later on give a quick reminder to which occurrence the event was related.


![Adding a new event](Addevent.png)

After choosing the type of activity, one should fill in the data about this activity. Different types of activities have different fields to be filled in, for example, after choosing tillage one should fill in the type of tillage, how the tillage was implemented and what the depth of the tillage was. Required option fields are marked with a star sign (*).

Here are some notes that generally apply to all of the activities and that are good to keep in mind when filling in the form:

- Every event has to be logged in as its own event. In other words, even though some of the events can happen simultaneously and can in one’s mind be summarised as one event, it may require two or more records in application. For example tillage and sowing should be handled separately even in reality they happened simultaneously or back to back.

- Pay attention to the units and in which form the application wants the numeric values. Units can change within the same form as some fields may request for example kg/ha while other fields may require t/ha. However, the desired unit is stated in the field so guessing is not required. It also doesn’t matter if the decimal point is indicated with “.” or “,”, the application recognizes both practices.

- The application indicates with red border lines, if some required value is missing from the form. It is not usually required to fill in all of the fields and some of the fields may be difficult even for farmers to fill in. However, we hope that the user tries to fill in the events with care and as accurately as they are able to. This ensures that the data is valid for future usage in  research purposes.

- **Events can be modified after saving them**, so there is no need to stress about doing something irreversible.


![Example of the view for filling in the (tilling) event](eventexample_1.png)


### Clone event
Any event can be cloned, and as the action implies, it creates a new event holding the same information as the event it was cloned from. However, like any other event, this new cloned event can be modified. This is practical for instance in situations wherein the same field management events have been conducted to several blocks. By cloning the event, the user can then easily change a block and save the record in order to have the same management event for several sites. The *Clone event* button is found next to the *Add event* button.


### Delete event
When choosing an event from the summary table, it is possible to edit the chosen event. Within this editing option, it is also possible to delete the event.




### Purpose of the application
The application is created to collect data of the field management events in a uniform way. The data collected through this application is used in the ecosystem models with a purpose to simulate carbon and other greenhouse gas exchanges in the field. Field management events play a key role in estimating such matters in agricultural environments.


### Contact information

Henri Kajasilta      henri.kajasilta@fmi.fi      [Eng / Fin]  
Istem Fer              istem.fer@fmi.fi              [Eng]


You may contact us, if you are facing one or several of the following situations:

- Add (your) site to the application  
- Reset the site/user password  
- Report a possible bug in the application  
- You want to request a feature that is currently not available  
- You want to know more about the app

Regarding the new features and bug fixes, you can also fill in a github issue [**here**](https://github.com/Ottis1/fieldactivity/issues) without contacting us.
