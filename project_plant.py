def code_1():
    import matplotlib.pyplot as plt
    import pandas as pd
    import matplotlib.dates as mdates
    from datetime import datetime

    # Define the tasks and their start and end dates
    tasks = [
        {"Task": "Research and Tool Selection", "Start": "2024-03-01", "End": "2024-03-07"},
        {"Task": "Understanding RSL and RSL* Grammar", "Start": "2024-03-08", "End": "2024-03-15"},
        {"Task": "Initial Design", "Start": "2024-03-16", "End": "2024-03-21"},
        {"Task": "Implementing the Lexer (RSL)", "Start": "2024-03-22", "End": "2024-04-14"},
        {"Task": "Developing the Parser (RSL)", "Start": "2024-03-22", "End": "2024-04-14"},
        {"Task": "Extending Lexer/Parser for RSL*", "Start": "2024-04-01", "End": "2024-04-23"},
        {"Task": "Testing Lexer and Parser", "Start": "2024-04-16", "End": "2024-04-26"},
        {"Task": "Basic Typechecking", "Start": "2024-04-26", "End": "2024-05-31"},
        {"Task": "Advanced Typechecking", "Start": "2024-05-11", "End": "2024-06-18"},
        {"Task": "Refinement", "Start": "2024-06-11", "End": "2024-07-02"},
        {"Task": "Documentation", "Start": "2024-07-03", "End": "2024-07-31"},
        {"Task": "Final Testing and Wrap-up", "Start": "2024-07-10", "End": "2024-07-17"}
    ]

    # Convert to DataFrame
    df = pd.DataFrame(tasks)

    # Convert start and end dates from string to datetime
    df['Start'] = pd.to_datetime(df['Start'])
    df['End'] = pd.to_datetime(df['End'])

    # Calculate durations
    df['Duration'] = df['End'] - df['Start']

    # Plotting
    fig, ax = plt.subplots(figsize=(10, 8))

    # Generate the bars for each task
    start_dates = df['Start']
    end_dates = df['End']
    tasks = df['Task']

    # Generate a color for each task
    colors = plt.cm.tab20c(range(len(tasks)))

    for i, task in enumerate(tasks):
        start = mdates.date2num(start_dates[i])
        end = mdates.date2num(end_dates[i])
        ax.barh(task, end - start, left=start, color=colors[i])

    # Set the y-axis to have the tasks in the order they were added
    ax.set_yticks(range(len(tasks)))
    ax.set_yticklabels(tasks)

    # Format the dates on the x-axis
    ax.xaxis_date()
    fig.autofmt_xdate()

    ax.set_title('Project Gantt Chart')
    ax.set_xlabel('Time')
    ax.set_ylabel('Task')

    plt.tight_layout()

    # Save the chart as a file
    plt.savefig("gantt_chart.png")

    # plt.show()

def code_2():
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    from datetime import datetime

    # Define the project phases with their start and end dates
    phases = {
        "Foundation in RSL and RSL*": ["2024-02-01", "2024-02-28"],
        "Exploration of F# and Scala": ["2024-03-01", "2024-03-07"],
        "Identify and Work on Problematic Constructs ": ["2024-03-08", "2024-03-14"],
        "Experimentation with Constructs Handling": ["2024-03-10", "2024-03-21"],
        "Build AST(Design)": ["2024-03-22", "2024-04-05"],
        "IIterative Development of Lexer, Parser, and Typechecker": ["2024-03-06", "2024-05-01"],
        "Integration of Advanced Constructs": ["2024-04-25", "2024-06-15"],
        "Documentation and Final Testing": ["2024-07-01", "2024-07-31"]
    }

    # Convert dates to datetime objects
    for phase, dates in phases.items():
        phases[phase] = [datetime.strptime(date, "%Y-%m-%d") for date in dates]

    # Initialize the figure and axis
    fig, ax = plt.subplots(figsize=(10, 6))

    # Plot each phase
    for i, (phase, dates) in enumerate(phases.items(), start=1):
        ax.plot(dates, [i] * 2, marker='o', markersize=10)
        ax.text(dates[0], i, f" {phase}", verticalalignment='center', fontsize=9)

    # Set the Y-axis
    ax.set_yticks(range(1, len(phases) + 1))
    ax.set_yticklabels([])  # Hide the y-tick labels

    # Format the date on the x-axis
    ax.xaxis.set_major_locator(mdates.MonthLocator())
    # ax.xaxis.set_major_locator(mdates.WeekdayLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b\n%Y"))
    # ax.xaxis.set_major_formatter(mdates.DateFormatter("%U\n%b %d"))
    ax.xaxis.set_minor_locator(mdates.DayLocator())
    # Adding grid lines for weeks
    ax.grid(axis='x', color='gray', linestyle='--', which='major', linewidth=0.5)


    plt.title('Gantt Chart for Typechecker Development Project Plan')
    plt.xlabel('Timeline')
    plt.grid(axis='x', color='gray', linestyle='--', linewidth=0.5)

    # Tight layout to use space effectively
    plt.tight_layout()

    # Save the chart
    plt_path = "RSL/Docs/gantt_chart_typechecker_project.png"
    plt.savefig(plt_path)

    # plt_path

def code_3():
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates
    import matplotlib.ticker as ticker
    import datetime

    class CustomWeekFormatter(ticker.Formatter):
        def __init__(self, start_week_offset=1):
            self.start_week_offset = start_week_offset
        
        def __call__(self, x, pos=None):
            # Convert ordinal to datetime
            dt = mdates.num2date(x)
            # Get week number and adjust by start_week_offset
            week_num = int(dt.strftime("%U")) + self.start_week_offset
            # Get month and day
            month = dt.strftime('%b')
            day = int(dt.strftime('%d')) - 1
            # Return formatted string
            return f'{week_num}\n({month} {day:02d})'

    # Define the project phases with their start and end dates
    phases = {
        "Study of RSL and RSL*": ["2024-02-01", "2024-02-29"],
        "Learning F# and Scala": ["2024-03-01", "2024-03-07"],
        "Identify and Work on Problematic Constructs ": ["2024-03-08", "2024-03-14"],
        "Experimentation Handling Constructs in Both Languages": ["2024-03-10", "2024-03-21"],
        "Build AST(Design)": ["2024-03-22", "2024-04-07"],
        "Iterative Development of Lexer, Parser, and Typechecker": ["2024-04-08", "2024-05-31"],
        "Documentation and Final Testing": ["2024-07-01", "2024-08-04"]
    }

    # Convert dates to datetime objects and calculate durations
    for phase, dates in phases.items():
        start, end = [datetime.datetime.strptime(date, "%Y-%m-%d") for date in dates]
        phases[phase] = [start + datetime.timedelta(days=1), end + datetime.timedelta(days=1), (end - start).days + 1]

    # Initialize the figure and axis
    fig, ax = plt.subplots(figsize=(22, 10))

    # Plot each phase with rectangular bars
    for i, (phase, (start, end, duration)) in enumerate(phases.items(), start=1):
        ax.barh(i, duration, left=start, height=0.4, align='center', edgecolor='black')
        ax.text(start, i, f"", 
                va='center', ha='left', fontsize=9)

    # Adjust chart margins to prevent overlap
    plt.subplots_adjust(left=0.3)
    
    # Set the Y-axis
    ax.set_yticks(range(1, len(phases) + 1))
    ax.set_yticklabels(phases.keys())
    ax.invert_yaxis()  # Invert y axis so that the first phase is on top

    # Format the date on the x-axis to show week numbers
    # ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_major_locator(mdates.WeekdayLocator())
    # ax.xaxis.set_major_formatter(mdates.DateFormatter("%b\n%Y"))
    ax.xaxis.set_major_formatter(CustomWeekFormatter())
    # ax.xaxis.set_minor_locator(mdates.DayLocator())

    # Adding grid lines for better readability
    ax.grid(axis='x', color='gray', linestyle='--', which='major', linewidth=0.5)
    # ax.grid(axis='x', color='gray', linestyle=':', which='minor', linewidth=0.5)

    plt.title('Gantt Chart for Typechecker Development Project Plan')
    plt.xlabel('Timeline (Week #)')
    plt.ylabel('Project Phases')

    # Tight layout to use space effectively
    plt.tight_layout()

    # Save and display the chart
    plt_path = "RSL/Docs/gantt_chart_typechecker_project_bars.png"
    plt.savefig(plt_path)
    # plt.show()

def code_4():
    from datetime import datetime, timedelta
    import matplotlib.pyplot as plt
    import matplotlib.dates as mdates

    # Define the project phases with their start and end dates
    phases = {
       "Foundation in RSL and RSL*": ["2024-02-01", "2024-02-28"],
        "Exploration of F# and Scala": ["2024-03-01", "2024-03-07"],
        "Identify and Work on Problematic Constructs ": ["2024-03-08", "2024-03-14"],
        "Experimentation with Constructs Handling": ["2024-03-10", "2024-03-21"],
        "Build AST(Design)": ["2024-03-22", "2024-04-05"],
        "IIterative Development of Lexer, Parser, and Typechecker": ["2024-04-06", "2024-05-01"],
        "Integration of Advanced Constructs": ["2024-04-25", "2024-06-15"],
        "Documentation and Final Testing": ["2024-07-01", "2024-07-31"]
    }

    # Custom start date for week calculation
    custom_start_date = datetime(2024, 2, 1)

    # Convert dates to datetime objects, calculate durations and custom week numbers
    for phase, dates in phases.items():
        start, end = [datetime.strptime(date, "%Y-%m-%d") for date in dates]
        duration = (end - start).days + 1
        # Calculate the week number by finding the difference from the custom start date
        week_number = ((start - custom_start_date).days // 7) + 1
        phases[phase] = [start, end, duration, week_number]

    # Initialize the figure and axis
    fig, ax = plt.subplots(figsize=(12, 8))

    # Plot each phase with rectangular bars
    for i, (phase, (start, end, duration, week_number)) in enumerate(phases.items(), start=1):
        ax.barh(i, duration, left=start, height=0.4, align='center', edgecolor='black')
        # Annotate the bar with the custom week number
        ax.text(start, i, f"Week {week_number}: {phase}", va='center', ha='left', fontsize=9)

    # Adjust chart margins and settings as before
    plt.subplots_adjust(left=0.3)
    # Set the y-axis
    ax.set_yticks(range(1, len(phases) + 1))
    ax.set_yticklabels([f"{phase} (Week {info[3]})" for phase, info in phases.items()])
    ax.invert_yaxis()

    # Format the x-axis
    ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%U"))
    ax.grid(axis='x', which='major', linestyle='--')

    plt.title('Gantt Chart with Custom Week Numbering')
    plt.xlabel('Timeline')
    plt.ylabel('Project Phases')

    plt.tight_layout()
    plt.savefig("RSL/Docs/gantt_chart_custom_weeks.png")

code_3()
