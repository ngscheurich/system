package cmd

import (
	"fmt"
	"os"
	"os/exec"
	"path"
	"strings"

	"github.com/charmbracelet/huh"
	"github.com/spf13/cobra"
)

var themeCmd = &cobra.Command{
	Use:   "theme",
	Short: "Change user theme",
	Run: func(cmd *cobra.Command, args []string) {
		theme := runForm()
		linkTheme(theme)
		loadKittyTheme()
		reloadKittyConf()
		loadTmuxTheme()
	},
}

func runForm() string {
	var opts []huh.Option[string]
	var theme string

	for _, t := range getThemes() {
		opts = append(opts, huh.NewOption(t, t))
	}

	f := huh.NewForm(
		huh.NewGroup(
			huh.NewSelect[string]().
				Options(opts...).
				Title("Themes").
				Value(&theme),
		),
	)

	err := f.Run()
	if err != nil {
		fmt.Println(err)
	}

	return theme
}

func getThemes() []string {
	var themes []string
	dir := fmt.Sprintf("%s/%s", SystemDir(), "themes")
	ents, err := os.ReadDir(dir)
	if err != nil {
		panic(err)
	}

	for _, e := range ents {
		if !strings.HasPrefix(e.Name(), ".") {
			themes = append(themes, e.Name())
		}
	}

	return themes

}

func linkTheme(theme string) {
	orig := path.Join(SystemDir(), "themes", theme)
	os.Remove(themePath())
	err := os.Symlink(orig, themePath())
	if err != nil {
		panic(err)
	}
}

func loadKittyTheme() {
	conf := path.Join(themePath(), "kitty.conf")
	cmd := exec.Command("kitty", "@", "set-colors", "--all", "--configured", conf)
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func reloadKittyConf() {
	conf := path.Join(os.Getenv("XDG_CONFIG_HOME"), "kitty", "kitty.conf")
	cmd := exec.Command("kitty", "@", "load-config", conf)
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func loadTmuxTheme() {
	script := path.Join(themePath(), "tmux.sh")
	cmd := exec.Command(script)
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func themePath() string {
	return path.Join(os.Getenv("HOME"), ".theme")
}

func init() {
	rootCmd.AddCommand(themeCmd)
}
