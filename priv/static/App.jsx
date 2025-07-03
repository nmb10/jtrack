const sessionColors = [
  '#d33682',
  '#93a1a1',
  '#b58900',
  '#6c71c4',
  '#586e75',
  '#657b83',
  '#dc322f',
  '#268bd2',
  '#839496',
  '#2aa198',
  '#859900',
  '#d33682',
  '#93a1a1',
  '#b58900',
  '#6c71c4',
  '#586e75',
  '#657b83',
  '#dc322f'
]

const { useRef, useState, useLayoutEffect, useEffect, useCallback } = React

const copyToClipboard = (text) => {
  navigator.clipboard.writeText(text).then(
    function () {
      console.log('Async: Copying to clipboard was successful!')
    },
    function (err) {
      console.error('Async: Could not copy text: ', err)
    })
}

const toggleIssue = (issue) => {

  const requestOptions = {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(issue)
  }

  fetch('/api/v1/issues', requestOptions)
    .then(response => response.json())
    .then(data => {
      document.getElementById('app')
        .dispatchEvent(new CustomEvent('issues.fetch', {}))
    })
}

const IssuesComponent = ({ issueList, inProgressIssue}) => {

  const issuesBlock = issueList.map((issue, index) => <IssueComponent key={index} issue={issue} inProgressIssue={inProgressIssue} />)

  return (
    <React.Fragment>
      {issuesBlock}
    </React.Fragment>
  )
}

const IssueComponent = ({issue, inProgressIssue}) => {
  let prefix = ''
  let background = ''
  let styles = {paddingLeft: '5px', borderBottom: '2px solid blue', borderRadius: '4px'}
  let buttonTitle = ''
  if (inProgressIssue && issue.key === inProgressIssue.key) {
    styles.backgroundColor = "#2aa198"
    buttonTitle = 'Stop'
  } else {
    buttonTitle = 'Start'
  }

  return (
    <tr>
      <td style={styles}>
        <button onClick={() => copyToClipboard(issue.key)}
                title="Copy issue key to clipboard"
                style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
          CK
        </button> |
        <button onClick={() => copyToClipboard(issue.url)}
                title="Copy issue URL to clipboard"
                style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
          CU
        </button> |
        <button onClick={() => copyToClipboard(issue.key + ': ' + issue.title)}
                title="Copy issue title to clipboard"
                style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
          CT
        </button>
        <br />
        <a href={issue.url}>
          {issue.key}
        </a>: {issue.title}
      </td>
      <td style={styles}>
        <button onClick={() => toggleIssue(issue)}>
          {buttonTitle}
        </button>
      </td>
    </tr>
  )
}

const CurrentIssueWidget = ({issue}) => {

  let styles = {
    paddingLeft: '5px',
    borderRadius: '4px',
    fontSize:'24px'}
  let styles1 = {
    paddingLeft: '5px',
    borderRadius: '4px'}
  const tableStyles = {
    width: "840px",
    display: "block",
    backgroundColor: "#2aa198",
    position: "fixed",
    top: "2px",
    border: "solid gray 4px"}

  const issueTitle = issue.title.substring(0, 28) + '...'

  const ret = (
    <table style={tableStyles}>
      <tbody>
        <tr>
          <td style={styles}>
            <button onClick={() => copyToClipboard(issue.key)}
                    title="Copy issue key to clipboard"
                    style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
              CK
            </button> |
            <button onClick={() => copyToClipboard(issue.url)}
                    title="Copy issue URL to clipboard"
                    style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
              CU
            </button> |
            <button onClick={() => copyToClipboard(issue.key + ': ' + issue.title)}
                    title="Copy issue title to clipboard"
                    style={{ float: 'left', margin: 0, paddingLeft: '5px', paddingRight: '5px' }}>
              CT
            </button>
            <br />
            <a href={issue.url} title={issue.title}>
              {issue.key}: {issueTitle}
            </a>
          </td>
          <td style={styles1}>
            <button onClick={() => toggleIssue(issue)}>
              Stop time tracking
            </button>
          </td>
        </tr>
      </tbody>
    </table>
  )
  return ret
}

const ConfigWidget = () => {
  const [isOpened, setIsOpened] = useState(false)

  const [featuredIssues, setFeaturedIssues] = useState([])
  const [hoursLimit, setHoursLimit] = useState(8)
  const [jiraBearer, setJiraBearer] = useState('')
  const [jiraDomain, setJiraDomain] = useState('')

  useEffect(() => {
    const requestOptions = {}
    fetch('/api/v1/config', requestOptions)
      .then(response => response.json())
      .then(configData => {
        setHoursLimit(configData.hours_limit)
        setJiraBearer(configData.jira_bearer)
        setJiraDomain(configData.jira_domain)
    })
  }, [])

  const onJiraBearerChange = (event) => {
    // FIXME: Send to server.
    const requestOptions = {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({jira_bearer: event.target.value})
    }
    fetch('/api/v1/config', requestOptions)
      .then(response => response.json())
      .then(data => {
        setJiraBearer(data.jira_bearer)
      })
      // FIXME: On error?
  }

  const onJiraDomainChange = (event) => {
    // FIXME: Send to server.
    const requestOptions = {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({jira_domain: event.target.value})
    }
    fetch('/api/v1/config', requestOptions)
      .then(response => response.json())
      .then(data => {
          setJiraDomain(data.jira_domain)
      })
  }

  const onHoursLimitChange = (event) => {
    const requestOptions = {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({hours_limit: event.target.value})
    }
    fetch('/api/v1/config', requestOptions)
      .then(response => response.json())
      .then(data => {
          setHoursLimit(data.hours_limit)
      })
  }

  let content
  if (isOpened) {
    content = <div style={{border: "solid blue 1px", backgroundColor: "wheat"}}>
      <a href="#" onClick={() => setIsOpened(false)} title="Close config options dialog">Close</a>
      <fieldset>
          <label htmlFor="hours-limit">
          Daily limit (stop tracker when the limit reached):
          </label>
          <input id="hours-limit" onChange={onHoursLimitChange} type="number" value={hoursLimit} />
      </fieldset>
      <fieldset>
          <label htmlFor="jira-upgrade">When to log hours to jira: FIXME:</label>
          <select name="jira-upgrade" id="jira-upgrade">
            <option value="on-daily-limit-reach" >on daily limit reach</option>
            <option value="manually">manually</option>
          </select>
      </fieldset>
      <fieldset>
        <legend>Jira setup:</legend>
        Jira bearer: <input type="text" onChange={onJiraBearerChange} value={jiraBearer} />
        Jira domain: <input type="text" onChange={onJiraDomainChange} value={jiraDomain} />
        <button onClick={() => alert('FIXME:')}>Test connection</button>
      </fieldset>
    </div>
  } else {
    content = <a href="#" onClick={() => setIsOpened(true)} title="Show config options">Config</a>
  }
  return content
}


const WorklogSession = ({ date, session, elemIndex, totalElems }) => {
  // session example:
  // [ {"key": "JT-11", ...}, [ "2025-03-07T08:22:28", 123, true],
  const [issue, [sessionId, sessionStartTime, sessionMinutes, sessionSaved]] = session
  let ret
  const hours = Math.floor(sessionMinutes / 60)
  let minutes = sessionMinutes % 60
  const issueTitle = issue.title.substring(0, 50) + '...'

  const colorIndex = totalElems - 1 - elemIndex
  if (hours === 0 && minutes === 0) {
      ret = ''
  } else {
    if (sessionSaved) {
      ret = (
        <tr style={{borderLeft: "10px " + sessionColors[colorIndex] + " solid", borderTop: "10px " +  sessionColors[colorIndex] + " solid"}} id={"lid-" + sessionId} className="worklog-session-li">
          <td>
          </td>
          <td>
            <a href={issue.url}>
              {issue.key}: {issueTitle}
            </a>
          </td>
          <td>
            {hours}h. {minutes}m. &#9989;
          </td>
          <td></td>
        </tr>
      )
    } else {
      const borderLeft = "10px " + sessionColors[colorIndex] + " solid"
      const borderTop = "10px " + sessionColors[colorIndex] + " solid"
      ret = (
        <tr style={{borderLeft: borderLeft, borderTop: borderTop}} className="worklog-session-li">
          <td>
            <button style={{padding:"0px", margin:"0", width:"25px", height:"30px"}}>&uarr;</button>
            <br />
            <button style={{padding:"0px", margin:"0", width:"25px", height:"30px"}}>&darr;</button>
          </td>
          <td>
            <a href={issue.url}>
              {issue.key}: {issueTitle}
            </a>
          </td>
          <td>
            {sessionStartTime}/{hours}h. {minutes}m.
          </td>
          <td>
            <button title="Save session time to jira"
                    onClick={() => saveSession(sessionId, sessionStartTime, sessionMinutes, issue.key)}>
              Save
            </button>
          </td>
        </tr>
      )
    }
  }
  return ret
}


const saveSession = (sessionId, startedAt, minutes, issueKey) => {
  /*
   * Saves issue time to jira.
   *
   */

  const requestOptions = {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      session_id: sessionId,
      started_at: startedAt,
      issue_key: issueKey,
      minutes: minutes})
  }
  fetch('/api/v1/sessions', requestOptions)
    .then(response => response.json())
    .then(data => {
      document.getElementById('app')
        .dispatchEvent(new CustomEvent('issues.fetch', {}))
    })
}


const WeekDaySessions = ({ date, sessions }) => {

  const jtrackWorklogItems = sessions.map((elem, index) => <WorklogSession key={index} date={date} session={elem} elemIndex={index} totalElems={sessions.length} />)
    //sessions
    //.map((elem, index) => renderSession(date, elem, index, sessions.length))
    //.join("\n")
  let issueBlocks = []

  let blockWidth
  let temp
  let issue
  let hours
  let minutes
  let colorIndex
  let backgroundColor
  let totalMinutes
  for (const [i, workSession] of sessions.entries()) {
      totalMinutes = workSession[1][2]
      hours = Math.floor(totalMinutes / 60)
      minutes = totalMinutes % 60
      blockWidth = totalMinutes + 'px'
      issue = workSession[0]
      colorIndex = sessions.length - 1 - i
      backgroundColor = sessionColors[colorIndex]
      temp = (
        <div key={i} title="{issue} ({hours}h {minutes}m)" id="work-session-{workSession[1][0]}" style={{float:"left", "width": blockWidth, height: "40px", border:"2px black solid", backgroundColor: backgroundColor}}>&nbsp;</div>
      )
      issueBlocks = [temp, ...issueBlocks]
  }

  let ret = <div>---</div>
  if (sessions.length > 0) {
    ret = (
      <div>
        <div style={{height: "40px", width: "490px", paddingLeft: "4px", paddingRight:"4px", backgroundColor: "gray"}}>
          {issueBlocks}
        </div>
        <table style={{borderCollapse: "collapse"}}>
          <tbody>
          {jtrackWorklogItems}
          </tbody>
        </table>
      </div>
    )
  }
  return ret
}

const WeekDayStatComponent = ({weekDayStat}) => {
  // weekDayStat example:
  // ["2025-03-07", [ [ "JT-12", [ "2025-03-07T08:22:28", 123 ] ],
  //
  const date = weekDayStat[0]
  const dateStat = weekDayStat[1]
  let dayTotalMinutes = 0
  for (const issueLog of dateStat) {
    dayTotalMinutes += issueLog[1][2]
  }

  const dayHours = Math.floor(dayTotalMinutes / 60)
  let dayMinutes = dayTotalMinutes % 60
  const formattedDate = formatDate(new Date(date))

  let weekAction
  const hasUnsavedIssue = weekDayStat[1].some(x => x[1][3] == false)

  const saveDay = (date1) => {
    console.log(date1)
  }

  if (weekDayStat[1].length == 0) {
    weekAction = ''
  } else if (hasUnsavedIssue) {
    weekAction = (
      <button title="Save all unsaved sessions to jira"
              onClick={() => saveDay(date)}>
        Save (FIXME:)
      </button>
    )
  } else {
    // All issues for that day are saved.
    weekAction = <React.Fragment>&#9989;</React.Fragment>
  }
  return (
    <ul>
      <li>
        {formattedDate} ({dayHours}h. {dayMinutes}m.){weekAction}
        <WeekDaySessions date={date} sessions={dateStat} />

      </li>
    </ul>
  )
  return ret
}


const IssueListWidget = ({ featuredIssues, issues, inProgressIssue, currentRowIndex, stat, jtrackWorklog }) => {

  if (inProgressIssue) {
    changeFavicon('⏲')
  } else {
    changeFavicon('🏝')
  }

  const requestOptions = {}
  const currentIssue = null

  let totalMinutes = 0
  for (const mins of Object.values(stat)) {
    totalMinutes += mins
  }
  const todayHours = Math.trunc(totalMinutes / 60)
  const todayMinutes = totalMinutes % 60
  let todayStatBlock
  if (inProgressIssue === null) {
    todayStatBlock = <h2>Today: {todayHours}h {todayMinutes}m</h2>
  } else {
    todayStatBlock = <h2 style={{backgroundColor: "#859900", border: "3px solid white", borderRadius: "4px"}}>Today: {todayHours}h {todayMinutes}m</h2>
  }

  let jtrackWeekAmount = 0
  let jtrackPreviousWeekAmount = 0
  let jiraWeekAmount = 0
  let jiraPreviousWeekAmount = 0

  const thisWeekStartDate = new Date()
  const distance = thisWeekStartDate.getDay() - 1
    thisWeekStartDate.setDate(thisWeekStartDate.getDate() - distance)
    thisWeekStartDate.setHours(0, 0, 0, 0)

  const previousWeekStartDate = new Date()
  const distance1 = thisWeekStartDate.getDay() - 8
  previousWeekStartDate.setDate(thisWeekStartDate.getDate() + distance1)
  previousWeekStartDate.setHours(0, 0, 0, 0)

  let jtrackDate
  let jiraLogged
  let jiraStartTime

  let thisWeekWorklogItems = []
  let previousWeekWorklogItems = []
  for (let jtrackWorklogItem of jtrackWorklog) {
    jtrackDate = new Date(jtrackWorklogItem[0])
    if (jtrackDate >= thisWeekStartDate) {
      for (let session of jtrackWorklogItem[1]) {
        jtrackWeekAmount += session[1][2]
        jiraLogged = session[1][3]
        if (jiraLogged) {
          jiraWeekAmount += session[1][2]
        }
      }
      thisWeekWorklogItems.push(jtrackWorklogItem)
    } else if (jtrackDate >= previousWeekStartDate) {
      for (let session of jtrackWorklogItem[1]) {
        jtrackPreviousWeekAmount += session[1][2]
        jiraLogged = session[1][3]
        if (jiraLogged) {
          jiraWeekAmount += session[1][2]
        }
      }
      previousWeekWorklogItems.push(jtrackWorklogItem)
    }
  }

  const jtrackWeekHours = Math.floor(jtrackWeekAmount / 60)
  const jtrackWeekMinutes = jtrackWeekAmount % 60

  const jtrackPreviousWeekHours = Math.floor(jtrackPreviousWeekAmount / 60)
  const jtrackPreviousWeekMinutes = jtrackPreviousWeekAmount % 60

  const jiraWeekHours = Math.floor(jiraWeekAmount / 60)
  const jiraWeekMinutes = jiraWeekAmount % 60

  const jiraPreviousWeekHours = Math.floor(jiraPreviousWeekAmount) / 60
  const jiraPreviousWeekMinutes = jiraPreviousWeekAmount % 60

  const thisWeekIssues = thisWeekWorklogItems.map((elem, index) => <WeekDayStatComponent key={index}  weekDayStat={elem} />)
  const previousWeekIssues = previousWeekWorklogItems.map((elem, index) => <WeekDayStatComponent key={index} weekDayStat={elem} />)
  return (
    <div className="row">
      <div className="column column-50">
        <h3>This week (jtrack): {jtrackWeekHours}h {jtrackWeekMinutes}m</h3>
        <h3>This week (jira): {jiraWeekHours}h {jiraWeekMinutes}m</h3>
        {thisWeekIssues}

        <hr />
        <h3>Previous week (jtrack): {jtrackPreviousWeekHours}h {jtrackPreviousWeekMinutes}m</h3>
        <h3>Previous week (jira): {jiraPreviousWeekHours}h {jiraPreviousWeekMinutes}m</h3>
        {previousWeekIssues}
      </div>
      <div className="column column-50">
          <div style={{width:"230px", display:"block", position:"fixed", top:"2px", right:"150px", backgroundColor:"white"}}>
            {todayStatBlock}
          </div>
          <table>
            <tbody style={{backgroundColor: "#daebe3"}}>
              <tr style={{backgroundColor: "#e4f0f5", borderRadius: "4px"}}>
                <td colSpan="2">
                  Featured issues:
                  <a href="#" title="Add featured issue">Add (FIXME:)</a>
                </td>
              </tr>
              <IssuesComponent issueList={featuredIssues} inProgressIssue={inProgressIssue}/>
            </tbody>
            <tbody>
              <tr style={{backgroundColor: "#e4f0f5", borderRadius: "4px"}}>
                <th colSpan="2">My issues:</th>
              </tr>

              <IssuesComponent issueList={issues} inProgressIssue={inProgressIssue} />
            </tbody>
          </table>
      </div>
    </div>
  )
}


const App = () => {
  const [inProgressIssue, setInProgressIssue] = useState({})
  const [issues, setIssues] = useState([])
  const [featuredIssues, setFeaturedIssues] = useState([])
  const [stat, setStat] = useState({})
  const [jtrackWorklog, setJtrackWorklog] = useState([])

  useEffect(() => {
    const requestOptions = {}
    fetch('/api/v1/issues', requestOptions)
      .then(response => response.json())
      .then(data => {
        setInProgressIssue(data.current_issue)
        setIssues(data.issues)
        setFeaturedIssues(data.featured_issues)
        setStat(data.stat)
        setJtrackWorklog(data.jtrack_worklog)
      })
  }, [])

  useEffect(() => {

    document.getElementById('app').addEventListener('issues.fetch', (event) => {
      const requestOptions = {}

      fetch('/api/v1/issues', requestOptions)
        .then(response => response.json())
        .then(data => {
          setInProgressIssue(data.current_issue)
          setIssues(data.issues)
          setFeaturedIssues(data.featured_issues)
          setStat(data.stat)
          setJtrackWorklog(data.jtrack_worklog)
        })
    })
  }, [])

  const fire = () => {
    document.getElementById('app').dispatchEvent(
      new CustomEvent(
        'issues.fetch',
        {}))
  }

  useLayoutEffect(() => {
    setInterval(fire, 10000)
  }, [])

  const currentRowIndex = 1
  let currentIssueBlock
  if (inProgressIssue && Object.keys(inProgressIssue).length > 0) {
      currentIssueBlock = <CurrentIssueWidget issue={inProgressIssue} />
  } else {
      currentIssueBlock = ''
  }
  return (
    <div>
      <ConfigWidget />
      {currentIssueBlock}
      <IssueListWidget
         featuredIssues={featuredIssues}
         issues={issues}
         inProgressIssue={inProgressIssue}
         currentRowIndex={currentRowIndex}
         stat={stat}
         jtrackWorklog={jtrackWorklog} />
    </div>
  )
}


const changeFavicon = (text) => {
  const canvas = document.createElement('canvas')
  canvas.height = 64
  canvas.width = 64
  const ctx = canvas.getContext('2d')
  ctx.font = '64px serif'
  ctx.fillText(text, 0, 64)

  const link = document.createElement('link')
  const oldLinks = document.querySelectorAll('link[rel="shortcut icon"]')
  oldLinks.forEach(e => e.parentNode.removeChild(e))
  link.id = 'dynamic-favicon'
  link.rel = 'shortcut icon'
  link.href = canvas.toDataURL()
  document.head.appendChild(link)
}


const formatDate = (date) => {
  const options = {
    weekday: "short",
    year: "numeric",
    month: "short",
    day: "numeric",
  }
  return date.toLocaleDateString('en-us', options)
}
