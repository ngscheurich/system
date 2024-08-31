package cmd

import (
	"fmt"
	"os"

	"github.com/charmbracelet/lipgloss"
	"github.com/charmbracelet/lipgloss/table"
	"github.com/spf13/cobra"
)

const (
	fg          = lipgloss.Color("7")
	purple      = lipgloss.Color("5")
	lightPurple = lipgloss.Color("13")
	green       = lipgloss.Color("2")
)

var configListCmd = &cobra.Command{
	Use:   "list",
	Short: "List config links",
	Run: func(cmd *cobra.Command, args []string) {
		re := lipgloss.NewRenderer(os.Stdout)

		var (
			HeaderStyle = re.NewStyle().Foreground(purple).Bold(true).Align(lipgloss.Center)
			CellStyle   = re.NewStyle().Padding(0, 1).Width(14).Foreground(fg)
			BorderStyle = lipgloss.NewStyle().Foreground(lightPurple)
		)

		cfg := GetProgManifest()

		var rows [][]string

		for _, e := range cfg.Entries {
			dest := GetEntryFsDest(e)
			fi, _ := os.Lstat(dest)
			isLinked := ""
			if fi != nil && fi.Mode()&os.ModeSymlink != 0 {
				isLinked = "✓"
			}
			rows = append(rows, []string{e.Name, isLinked, e.Dest})
		}

		t := table.New().
			Border(lipgloss.ThickBorder()).
			BorderStyle(BorderStyle).
			StyleFunc(func(row, col int) lipgloss.Style {
				var style lipgloss.Style

				if row == 0 {
					return HeaderStyle
				} else {
					style = CellStyle
				}

				if col == 1 {
					style = style.Width(11).Foreground(green).Align(lipgloss.Center)
				} else if col == 2 {
					style = style.Width(22)
				}

				return style
			}).
			Headers("PROGRAM", "LINKED?", "DESTINATION").
			Rows(rows...)

		fmt.Println(t)
	},
}

func init() {
	configCmd.AddCommand(configListCmd)
}
