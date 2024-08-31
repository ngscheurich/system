package cmd

import (
	"fmt"
	"os"

	"github.com/charmbracelet/huh"
	"github.com/spf13/cobra"
)

var configLinkCmd = &cobra.Command{
	Use:   "link",
	Short: "Add program config link(s)",
	Run: func(cmd *cobra.Command, args []string) {
		var opts []huh.Option[string]
		var names []string

		cfg := GetDotfilesConfig()

		for _, e := range cfg.Entries {
			dest := GetDotfileDest(e)
			_, err := os.Stat(dest)
			if err != nil {
				opts = append(opts, huh.NewOption(e.Name, e.Name))
			}
		}

		f := huh.NewForm(
			huh.NewGroup(
				huh.NewMultiSelect[string]().
					Options(opts...).
					Title("Configs").
					Value(&names),
			),
		)
		err := f.Run()
		if err != nil {
			fmt.Println(err)
		}

		for _, n := range names {
			for _, e := range cfg.Entries {
				if e.Name == n {
					path := fmt.Sprintf("%s/dotfiles/%s", SystemDir(), e.Path)
					dest := GetDotfileDest(e)
					err := os.Symlink(path, dest)
					if err != nil {
						fmt.Println(err)
					}
				}
			}
		}
	},
}

func init() {
	configCmd.AddCommand(configLinkCmd)
}
