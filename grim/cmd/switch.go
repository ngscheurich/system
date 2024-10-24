package cmd

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"

	"github.com/spf13/cobra"
)

var host string
var outdir string = fmt.Sprintf("%snix-build-result", os.TempDir())

var switchCmd = &cobra.Command{
	Use:   "switch",
	Short: "Switch to a new Nix system generation",
	Run: func(cmd *cobra.Command, args []string) {
		platform := runtime.GOOS
		nix_switch(platform, host)
	},
}

func nix_switch(platform string, host string) {
	var cmd *exec.Cmd
	if platform == "darwin" {
		cmd = darwin_switch_cmd(host)
	} else {
		cmd = nixos_switch_cmd(host)
	}
	run_cmd(cmd)
}

func run_cmd(cmd *exec.Cmd) {
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	if err != nil {
		fmt.Println(err)
	}
}

func darwin_switch_cmd(host string) *exec.Cmd {
	flake := fmt.Sprintf("%s#%s", SystemDir(), host)
	return exec.Command("darwin-rebuild", "switch", "--flake", flake)
}

func nixos_switch_cmd(host string) *exec.Cmd {
	flake := fmt.Sprintf("%s#%s", SystemDir(), host)
	return exec.Command("sudo", "nixos-rebuild", "switch", "--flake", flake)
}

func init() {
	hostname, err := os.Hostname()
	if err != nil {
		fmt.Println("Error:", err)
		return
	}

	switchCmd.Flags().StringVarP(&host, "host", "H", hostname, "Nix host")
	rootCmd.AddCommand(switchCmd)
}
