function fish_greeting; end

function current_weather
    set -l data /tmp/current_weather.json
    set -l cond ""

    switch (jq -r ".weather[0].main" $data)
        case Thunderstorm
            set -l cond ""
        case Drizzle
            set -l cond ""
        case Rain
            set -l cond ""
        case Snow
            set -l cond ""
        case Clear
            set -l cond ""
        case Clouds
            set -l cond ""
    end

    set -l temp (cat $data | jq ".main.temp" | xargs printf "%.*f\n" 0)

    echo "$cond $temp°"
end

function date_time
    set -l day (string replace " " 0 (date +"%e"))
    echo (date +"%a %b") $day (date +"%H:%M")
end

function get_weather
    set -l response (curl "https://api.openweathermap.org/data/2.5/weather?q=Denham+Springs&units=imperial&appid=$OPEN_WEATHER_API_KEY")
    echo $response > /tmp/current_weather.json
end

function now_playing
    set -l spotify_status (spotify status | head -n 1)
    set icon " "

    if ! set -q spotify_status
        return 1
    else if test -n (string match -r ".*paused." $spotify_status)
        set icon " "
    end

    set -l artist (spotify status artist)

    if test $artist = "The Mountain Goats"
        set -l artist "the Mountain Goats"
    end

    echo $icon (spotify status track) - $artist
end

function tmuxdir
    tmux new-session -s $(pwd | string split '/')[-1]
end
