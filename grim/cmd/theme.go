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

var theme string

var themeCmd = &cobra.Command{
	Use:   "theme",
	Short: "Change user theme",
	Run: func(cmd *cobra.Command, args []string) {
		if theme == "" {
			theme = runForm()
		}
		linkTheme(theme)
		reloadGhosttyConf()
		reloadTmuxTheme()
		reloadNvimTheme()
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
	).WithTheme(huh.ThemeBase16())

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

func reloadGhosttyConf() {
	cmd := exec.Command("/etc/system/scripts/reload-ghostty-conf.sh")
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func reloadTmuxTheme() {
	script := path.Join(themePath(), "tmux.sh")
	cmd := exec.Command(script)
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func reloadNvimTheme() {
	cmd := exec.Command("/etc/system/scripts/nvim-send.sh", "<Cmd>lua require('ngs.util').reload_theme(true)<CR>")
	err := cmd.Run()
	if err != nil {
		panic(err)
	}
}

func themePath() string {
	return path.Join(os.Getenv("HOME"), ".theme")
}

func init() {
	themeCmd.Flags().StringVarP(&theme, "theme", "t", "", "Theme name")
	rootCmd.AddCommand(themeCmd)
}
