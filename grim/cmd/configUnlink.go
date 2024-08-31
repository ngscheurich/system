package cmd

import (
	"fmt"
	"os"

	"github.com/charmbracelet/huh"
	"github.com/spf13/cobra"
)

var configUnlinkCmd = &cobra.Command{
	Use:   "unlink",
	Short: "Remove program config link(s)",
	Run: func(cmd *cobra.Command, args []string) {
		var opts []huh.Option[string]
		var names []string

		cfg := GetDotfilesConfig()

		for _, e := range cfg.Entries {
			dest := GetDotfileDest(e)
			_, err := os.Stat(dest)
			if err == nil {
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
					dest := GetDotfileDest(e)
					os.Remove(dest)
				}
			}
		}
	},
}

func init() {
	configCmd.AddCommand(configUnlinkCmd)
}
