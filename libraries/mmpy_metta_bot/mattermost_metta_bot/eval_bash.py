#!/usr/bin/env python

# Stub class for MeTTaLog
import io
import sys
import subprocess
import tempfile
from collections import defaultdict
import click
from pathlib import Path
import re
import logging
from mmpy_bot import Plugin, listen_to
from mmpy_bot import Bot, Settings
import os
import json
import traceback

# Configure logging
logging.basicConfig(level=logging.DEBUG)

# Map interpreter types to their respective BASH_BEFORE and BASH_AFTER values
INTERPRETER_CONFIGS = {
    "swipl": {
        "BASH_BEFORE": ["swipl", "-q", "-f"],
        "BASH_AFTER": ["-t", "halt"]
    },
    "metta": {
        "BASH_BEFORE": ["metta"],
        "BASH_AFTER": []
    },
    "mettalog": {
        "BASH_BEFORE": ["mettalog"],
        "BASH_AFTER": []
    }
}

# Dictionary to maintain MeTTaLog instances keyed by friendly names
friendly_name_to_metta = defaultdict(lambda: MeTTaLog())

def get_friendly_name(user_id=None, username=None, channel_id=None, channel_name=None, is_private=False):
    """
    Generates or retrieves a friendly name for a user or channel.
    """
    if is_private:
        return f"Private-{username or user_id}"
    if username:
        return username
    if channel_name:
        return channel_name
    return f"Channel-{channel_id}" if channel_id else f"User-{user_id}"

def generate_friendly_name(base_name, unique_suffix):
    """
    Generates a friendly name by appending a unique suffix if the name already exists.
    """
    if base_name not in friendly_name_to_metta:
        return base_name
    return f"{base_name}_{unique_suffix}"
    

def get_metta_instance(
    user_id=None, username=None, channel_id=None, channel_name=None, is_private=False, is_mention=False):
    """
    Retrieves or initializes the appropriate MeTTaLog instance based on friendly names.
    """
    friendly_name = get_friendly_name(
        user_id=user_id, username=username, channel_id=channel_id, channel_name=channel_name, is_private=is_private
    )
    unique_suffix = f"{user_id or channel_id}"
    key = generate_friendly_name(friendly_name, unique_suffix)

    # If no instance exists, create one
    if key not in friendly_name_to_metta:
        logging.debug(f"Creating new MeTTaLog instance for key: {key}")
        friendly_name_to_metta[key] = MeTTaLog(name=key)
    return friendly_name_to_metta[key]

class MeTTaLog:
    def __init__(self, interp_type="metta", name=None):
        self.set_interpreter(interp_type)
        self.name = name or "Unnamed Interpreter"

    def set_interpreter(self, interp_type):
        """Sets the interpreter type and updates configurations."""
        interp_type = interp_type.strip()
        if interp_type not in INTERPRETER_CONFIGS:
            raise ValueError(f"Invalid interp_type: {interp_type}")
        self.interp_type = interp_type
        self.BASH_BEFORE = INTERPRETER_CONFIGS[interp_type]["BASH_BEFORE"]
        self.BASH_AFTER = INTERPRETER_CONFIGS[interp_type]["BASH_AFTER"]

    def format_output(self, label, content):
        """
        Formats output depending on its length after trimming.
        """
        content = content.strip()
        if '\n' in content or len(content) > 80:
            return f"\n{label}```\n{content}\n```"
        else:
            return f"\n{label}`{content}`\n"

    def exec(self, code):
        """
        Executes a block of code by writing it to a temporary file and invoking the interpreter.
        Captures both stdout and stderr.
        """
        with tempfile.NamedTemporaryFile(delete=False, suffix=".metta", mode="w") as temp_file:
            temp_file.write(code)
            temp_file_path = temp_file.name

        try:
            command = self.BASH_BEFORE + [temp_file_path] + self.BASH_AFTER
            logging.debug(f"Executing command: {' '.join(command)}")
            process = subprocess.Popen(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            stdout, stderr = process.communicate()

            codewas = self.format_output("Execute:", code)
            captured_output = self.format_output("Captured Output:", stdout) if stdout.strip() else ""
            captured_error = self.format_output("Captured Errors:", stderr) if stderr.strip() else ""

            if process.returncode != 0:
                return codewas + captured_error

            return codewas + captured_output + captured_error
        except Exception as e:
            return self.format_output("Error: ", str(e))
        finally:
            try:
                os.remove(temp_file_path)
            except Exception as e:
                logging.error(f"Failed to remove temporary file: {e}")

    def eval(self, expression):
        """
        Evaluates an expression by writing it to a temporary file and invoking the interpreter.
        Captures both stdout and stderr.
        """
        expression = expression.strip()

        if expression.startswith('!'):
            query_code = expression + "\n"
        else:
            query_code = "!" + expression + "\n"

        with tempfile.NamedTemporaryFile(delete=False, suffix=".metta", mode="w") as temp_file:
            temp_file.write(query_code)
            temp_file_path = temp_file.name

        try:
            command = self.BASH_BEFORE + [temp_file_path] + self.BASH_AFTER
            logging.debug(f"Executing command: {' '.join(command)}")
            process = subprocess.Popen(
                command,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            stdout, stderr = process.communicate()

            codewas = self.format_output("Eval:", query_code)
            captured_result = self.format_output("Result:", stdout.strip()) if stdout.strip() else ""
            captured_error = self.format_output("Captured stderr:", stderr) if stderr.strip() else ""

            if process.returncode != 0:
                return codewas + captured_error

            return codewas + captured_result + captured_error
        except Exception as e:
            return self.format_output("Error: ", str(e))
        finally:
            try:
                os.remove(temp_file_path)
            except Exception as e:
                logging.error(f"Failed to remove temporary file: {e}")

class MeTTaBotReplPlugin(Plugin):
    """
    A plugin that logs all attributes of the Message object, evaluates commands,
    and handles edits by responding appropriately.
    """

    @listen_to(".*use (swipl|metta|mettalog)$", re.IGNORECASE)
    async def switch_interpreter(self, message, interp_type):
        """Switches the interpreter type for the current user/channel."""
        try:
            is_private = hasattr(message, 'channel_type') and message.channel_type == "D"  # Direct messages
            is_mention = self.driver.user_id in message.mentions  # Check if bot was mentioned

            # Determine the appropriate MeTTaLog instance
            metta_instance = get_metta_instance(
                user_id=message.user_id,
                username=message.sender_name,
                channel_id=message.channel_id,
                channel_name=message.channel_name,
                is_private=is_private,
                is_mention=is_mention
            )

            metta_instance.set_interpreter(interp_type.lower())
            response = f"Interpreter switched to `{interp_type}` for `{metta_instance.name}`."
            self.driver.reply_to(message, response)
        except ValueError as e:
            await self.driver.reply_to(message, f"Error: {str(e)}")
        except Exception as e:
            logging.error(f"Failed to switch interpreter: {e}")
            await self.driver.reply_to(message, "An error occurred while switching the interpreter.")

    async def respond_or_edit(self, message, response, edited=False):
        """
        Responds to a message or edits the bot's previous response if applicable.
        """
        if response is not None:  # Only respond if there's something to say
            try:
                thread_context = "in a thread" if message.root_id else "not in a thread"
                logging.debug(f"Preparing to reply {thread_context}.")

                if edited:
                    # Find the bot's last reply in the thread/channel
                    last_reply = self.driver.posts.get_posts_for_channel(
                        message.channel_id, params={'order': 'desc', 'limit': 10}
                    )
                    last_bot_post = next(
                        (post for post in last_reply['order']
                         if last_reply['posts'][post]['user_id'] == self.driver.bot_id),
                        None
                    )
                    if last_bot_post and 'message' in last_reply['posts'][last_bot_post]:
                        # Edit the last bot's post
                        logging.debug("Editing the bot's last post.")
                        self.driver.posts.update_post(last_bot_post, {'message': response})
                    else:
                        # Send a new message as a reply in a thread
                        logging.debug("Creating a new reply in a thread.")
                        self.driver.posts.create_post({
                            'channel_id': message.channel_id,
                            'message': response,
                            'root_id': message.root_id if message.root_id else None
                        })
                else:
                    # Send a new message in the same context
                    post_data = {
                        'channel_id': message.channel_id,
                        'message': response
                    }
                    if message.root_id:  # Only include root_id if the message is part of a thread
                        post_data['root_id'] = message.root_id
                        logging.debug("Replying in the same thread.")
                    else:
                        logging.debug("Replying in the main channel.")
                    self.driver.posts.create_post(post_data)
            except Exception as e:
                logging.error(f"Failed to respond or edit message: {e}")

    async def log_and_respond(self, message, edited=False):
        self.print_message_attributes(message)
        is_private = hasattr(message, 'channel_type') and message.channel_type == "D"  # Direct messages
        is_mention = self.driver.user_id in message.mentions  # Check if bot was mentioned

        # Determine the appropriate MeTTaLog instance
        metta_instance = get_metta_instance(
            user_id=message.user_id,
            username=message.sender_name,
            channel_id=message.channel_id,
            channel_name=message.channel_name,
            is_private=is_private
        )

        response, should_reply = self.evaluate_command(metta_instance, message.text.strip())
        if should_reply:
            await self.respond_or_edit(message, response, edited=edited)


    def evaluate_command(self, metta_instance, command: str) -> (str, bool):
        """
        Enhanced evaluation function to handle commands starting with '!', '(' and code blocks enclosed in triple backticks with 'metta'.
        """
        if not command:
            return None, False

        command = command.strip()

        try:
            # Check for code block enclosed in triple backticks with keyword 'metta'
            if '```metta' in command:
                code_block = command.split('```metta')[1].strip().split('```')[0]
                results = metta_instance.exec(code_block)
                return results, True

            # Evaluate expressions starting with backticks
            if '`!' in command:
                expression = command.split('`!')[1].strip().split('`')[0]
                result = metta_instance.eval(expression)
                return result, True

            if '`(' in command:
                expression = command.split('`(')[1].strip().split('`')[0]
                result = metta_instance.exec(expression)
                return result, True

            # Handle single-line commands starting with '!'
            if command.startswith('!'):
                expression = command.split('!')[1].strip().split('\n')[0]                
                result = metta_instance.eval(expression)
                return result, True

            # Handle commands wrapped in parentheses
            if command.startswith('(') and command.endswith(')'):
                result = metta_instance.exec(command)
                return result, True

        except Exception as e:
            return f"Error: {traceback.format_exc()}", True

        return None, False  # No valid command detected

    @listen_to(".*", needs_mention=False, message_edited=True)
    async def log_and_respond_edited_true(self, message):
        await self.log_and_respond(message, edited=True)

    @listen_to(".*", needs_mention=False, message_edited=False)
    async def log_and_respond_edited_false(self, message):
        await self.log_and_respond(message, edited=False)

    @listen_to("^hello_file$", re.IGNORECASE, needs_mention=True)
    async def hello_file(self, message):
        """Responds by uploading a text file."""
        file = Path("/tmp/hello.txt")
        file.write_text("Hello from this file!")
        await self.driver.reply_to(message, "Here you go", file_paths=[file])

    @listen_to("hello_click", needs_mention=True)
    @click.command(help="An example click command with various arguments.")
    @click.argument("POSITIONAL_ARG", type=str)
    @click.option("--keyword-arg", type=float, default=5.0, help="A keyword arg.")
    @click.option("-f", "--flag", is_flag=True, help="Can be toggled.")
    def hello_click(self, message, positional_arg: str, keyword_arg: float, flag: bool):
        """A click function documented via docstring"""
        response = (
            "Received the following arguments:\n"
            f"- positional_arg: {positional_arg}\n"
            f"- keyword_arg: {keyword_arg}\n"
            f"- flag: {flag}\n"
        )
        self.driver.reply_to(message, response)

    def print_message_attributes(self, message):
        """
        Logs all attributes of the Message object for debugging purposes.
        """
        print("Flattened Message Attributes:")
        print(json.dumps(message.__dict__, indent=4))

if __name__ == "__main__":
    # Start the bot with the custom settings
    bot = Bot(
        settings=Settings(
            MATTERMOST_URL="chat.singularitynet.io",
            MATTERMOST_PORT=443,
            BOT_TOKEN=os.getenv("MM_BOT_TOKEN") or (lambda: (_ for _ in ()).throw(ValueError("Environment variable 'MM_BOT_TOKEN' is not set")))(),
            BOT_TEAM="chat",
            SSL_VERIFY=True  # Maybe temporarily disable SSL verification for debugging
        ),
        plugins=[MeTTaBotReplPlugin()],
    )
    bot.run()

