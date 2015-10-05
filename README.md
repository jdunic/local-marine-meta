# README--Local marine meta-analysis

This is the repository containing code and data for the local marine meta-analysis that is trying to attribute local global drivers of biodiversity change.

### Getting started

Clone the repository to your computer by doing:

~~~
git clone https://github.com/jdunic/local-marine-meta
~~~

The raw data is contained in the directory `master_data`. 

### Important note for editting data
Data will be added, using git to track changes in data. Please try and make your commit messages meaningful, describing the purposes of your changes (e.g., correcting study ID value; quality controlled - checked; etc.,). You will most likely be editting the data in Excel. This will require an extra step (because computers are kind of stupid).

You can find and edit your .git/config file by doing (in a terminal):

~~~
cd path_to_local_meta_folder
~~~

Once there you can use the use the text editor nano to edit the .git/config file:

~~~
nano .git/config
~~~

In your .git/config file add the following:

~~~
[filter "cr"]
    clean = tr '\\r' '\\n'
    smudge = tr '\\n' '\\r'
~~~

Save and quit. You can check that your diffs are working. If you are finding that it looks like an entire csv is changing (deletions and additions that you didn't make -- i.e., massive amounts of red and green) then please post an issue to let me know. Tracking our specific changes is not useful if the entire file changes!

~~~
git diff master_data_sheet_of_choice.csv
~~~
