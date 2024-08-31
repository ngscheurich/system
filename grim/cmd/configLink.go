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

		cfg := GetProgManifest()

		for _, e := range cfg.Entries {
			dest := GetEntryFsDest(e)
			fi, _ := os.Lstat(dest)
			if fi == nil || fi.Mode()&os.ModeSymlink == 0 {
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
					path := fmt.Sprintf("%s/config/%s", SystemDir(), e.Path)
					dest := GetEntryFsDest(e)
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
