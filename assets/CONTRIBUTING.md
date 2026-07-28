# Assets

This directory contains assets used in the README.

The Gif files were generated from screen recordings using the following `ffmpeg` options:

```sh
ffmpeg \
    -ss 00:00 \
    -to 00:50 \
    -i <input-file> \
    -vf "\
        setpts=0.2*PTS, \
        fps=fps=1, \
        crop=2483:1382:0:54, \
        scale=1280:-1:flags=lanczos, \
        split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse" \
    -loop 0 \
    <output-file>.gif
```

The start and end time and the FPS reduction are based on Grafana's 5s automatic refresh.
With the start and end time, this should capture exactly 9 refreshes.
The FPS reduction speeds up the input video to show one refresh per second and reduces the framerate down to one frame per second, so that each refresh gets its own frame.

The crop is just what happened to work for my screen recording, cropping the viewport down to just the Grafana interface.

The split and paletegen/paletteuse construct a palette from the input video.

The `-loop 0` option, confusingly, tells `ffmpeg` to loop.
